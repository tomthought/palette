(ns palette.core
  (:require [clojure.string :as st]

            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format :as gformat])

            [utilis.types.number :refer [string->long string->double]]))

;;; Declarations

(declare add-percent
         rem-percent

         parse-rgb-string
         parse-hex-string
         parse-hsv-string

         hsv->rgba
         rgba->hsv

         clamp

         abs)

;;; Records

(defrecord ColorRGBA [r g b a])
(defrecord ColorHSV [h s v])

;;; Public

(defn rgb?
  [c]
  (boolean
   (if (string? c)
     (re-find #"^rgb" c)
     (instance? ColorRGBA c))))

(defn hex?
  [c]
  (boolean
   (and (string? c)
        (re-find #"^\#" c))))

(defn hsv?
  [c]
  (boolean
   (if (string? c)
     (re-find #"^hsv" c)
     (instance? ColorHSV c))))

(defn color-rgba
  ([c]
   (cond

     (string? c)
     (when-let [[r g b a] (not-empty
                           (cond
                             (rgb? c) (parse-rgb-string c)
                             (hex? c) (parse-hex-string c)
                             :else nil))]
       (color-rgba r g b a))

     (instance? ColorRGBA c) c
     (instance? ColorHSV c) (hsv->rgba c)

     :else nil))
  ([r g b & [a]]
   (let [a (or a 255)]
     (when (and r g b a)
       (ColorRGBA.
        (clamp r 0 255)
        (clamp g 0 255)
        (clamp b 0 255)
        (clamp (or a 255) 0 255))))))

(defn color-rgb
  ([c] (color-rgba c))
  ([r g b] (color-rgba r g b)))

(defn color-hsv
  ([c]
   (cond
     (string? c) (when (hsv? c)
                   (when-let [[h s v] (parse-hsv-string c)]
                     (color-hsv h s v)))
     (instance? ColorHSV c) c
     (instance? ColorRGBA c) (rgba->hsv c)
     :else nil))
  ([h s v]
   (when (and h s v)
     (ColorHSV.
      (clamp h 0 360)
      (clamp s 0 100)
      (clamp v 0 100)))))

(defn rgba->hsv
  [rgba]
  (let [{:keys [r g b a]} rgba
        r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)

        c-max (max r g b)
        c-min (min r g b)

        delta (- c-max c-min)

        h (cond
            (zero? delta) 0
            (= c-max r) (* 60 (mod (/ (- g b) delta) 6))
            (= c-max g) (* 60 (+ (/ (- b r) delta) 2))
            (= c-max b) (* 60 (+ (/ (- r g) delta) 4)))
        s (float (* 100 (if (zero? c-max) 0 (/ delta c-max))))
        v (float (* 100 c-max))]
    (color-hsv h s v)))

(defn hsv->rgba
  [hsv]
  (let [{:keys [h s v]} hsv
        s (float (/ s 100))
        v (float (/ v 100))
        c (* v s)
        x (* c (- 1.0 (abs (dec (mod (/ h 60) 2)))))
        m (- v c)

        [r g b] (cond
                  (<= 0 h 59) [c x 0]
                  (<= 60 h 119) [x c 0]
                  (<= 120 h 179) [0 c x]
                  (<= 180 h 239) [0 x c]
                  (<= 240 h 299) [x 0 c]
                  (<= 300 h 359) [c 0 x]
                  :else nil)]
    (color-rgb
     (* 255 (+ r m))
     (* 255 (+ g m))
     (* 255 (+ b m)))))

(defn color
  [c]
  (cond
    (rgb? c) (color-rgba c)
    (hex? c) (color-rgba c)
    (hsv? c) (color-hsv c)
    :else nil))

(defn lighten
  "Given color 'c' in either hex or rgb[a] format, lighten it by 'percent' [0 1]
  amount. The percentage added is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (lighten c 0.10))
  ([c percent]
   (when-let [color (color c)]

     (cond
       (instance? ColorRGBA color)
       (-> color
           (update :r add-percent percent)
           (update :g add-percent percent)
           (update :b add-percent percent))

       (instance? ColorHSV color)
       (update color :v #(clamp (+ % (* percent 100.0)) 0 100))

       :else nil))))

(defn darken
  "Given color 'c' in either hex or rgb[a] format, darken it by 'percent' [0 1]
  amount. The percentage removed is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (darken c 0.10))
  ([c percent] (lighten c (- percent))))

(defn transparent
  "Given color 'c' in either hex or rgb[a] format, set its opacity value to
  'opacity' [0 1]. By default it is set to 50%."
  ([c] (transparent c 0.50))
  ([c opacity]
   (when-let [color (color c)]
     (when (rgb? color)
       (assoc color :a opacity)))))

;;; Private

(defn- abs
  [x]
  #?(:clj (Math/abs (double x))
     :cljs (js/Math.abs (double x))))

(def ^:private hex-character-strings
  ["A" "B" "C" "D" "E" "F"
   "a" "b" "c" "d" "e" "f"
   "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"])

(def ^:private hex-characters
  #?(:clj (set (map first hex-character-strings))
     :cljs (set hex-character-strings)))

(defn- hex-character?
  [c]
  (boolean (hex-characters c)))

(defn- hex-digits
  [c]
  (->> c
       st/upper-case
       (filter hex-character?)
       (partition 2)
       (map (comp (fn [s]
                 #?(:clj (try (Integer/parseInt s 16)
                              (catch Exception e nil))
                    :cljs (js/parseInt s 16)))
               (partial st/join "")))))

(defn- clamp
  [x mn mx]
  (when (and x mn mx)
    (max (min x mx) mn)))

(defn- add-percent
  [c p]
  (int (clamp (+ c (* 255 p)) 0 255)))

(defn- rgb-digits
  [c]
  (let [digits (->> c
                    (re-seq #"[-+]?\d*\.\d+|\d+")
                    (map string->double))]
    (cond
      (= 3 (count digits)) (conj (vec digits) 255)
      (= 4 (count digits)) digits
      :else nil)))

(defn- parse-rgb-string
  [rgb-string]
  (rgb-digits rgb-string))

(defn- parse-hex-string
  [hex-string]
  (hex-digits hex-string))

(defn- hsv-digits
  [c]
  (let [digits (->> c
                    (re-seq #"[-+]?\d*\.\d+|\d+")
                    (map string->double))]
    (when (= 3 (count digits)) digits)))

(defn- parse-hsv-string
  [hsv-string]
  (hsv-digits hsv-string))
