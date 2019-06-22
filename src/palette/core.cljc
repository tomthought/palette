(ns palette.core
  (:require [clojure.string :as st]

            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format :as gformat])

            [palette.cie :as cie]

            [utilis.types.number :refer [string->long string->double]]
            [clojure.string :as str]))

;;; Declarations

(declare add-percent
         rem-percent

         parse-rgba
         parse-hex
         parse-hsv
         parse-lab

         rgba-string
         hex-string
         hsv-string
         lab-string

         hsv->rgba
         rgba->hsv
         rgba->lab

         clamp

         abs sqrt pow

         magnitude
         euclidean-distance)

;;; Conversions

(defn rgb?
  [c]
  (boolean (re-find #"^rgb" (str c))))

(defn hex?
  [c]
  (boolean (re-find #"^\#" (str c))))

(defn lab?
  [c]
  (boolean (re-find #"^lab" (str c))))

(defn hsv?
  [c]
  (boolean (re-find #"^hsv" (str c))))

(defn rgba->hsv
  [rgba]
  (let [[r g b a] (parse-rgba rgba)
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
    (hsv-string h s v)))

(defn rgba->lab
  [rgba]
  (when-let [^doubles lab (->> rgba parse-rgba double-array cie/rgb->lab)]
    (lab-string
     (aget lab 0)
     (aget lab 1)
     (aget lab 2))))

(defn hsv->rgba
  [hsv]
  (let [[h s v] (parse-hsv hsv)
        h (mod h 360)
        s (float (/ s 100))
        v (float (/ v 100))
        c (* v s)
        x (* c (- 1.0 (abs (dec (mod (/ h 60) 2)))))
        m (- v c)

        [r g b] (cond
                  (and (>= h 0) (< h 60)) [c x 0]
                  (and (>= h 60) (< h 120)) [x c 0]
                  (and (>= h 120) (< h 180)) [0 c x]
                  (and (>= h 180) (< h 240)) [0 x c]
                  (and (>= h 240) (< h 300)) [x 0 c]
                  (and (>= h 300) (< h 360)) [c 0 x]
                  :else (throw (ex-info "bad h value" {:h h})))]
    (rgba-string
     (* 255 (+ r m))
     (* 255 (+ g m))
     (* 255 (+ b m)))))

(defn hex->rgba
  [c]
  (->> (str c)
       (parse-hex)
       (apply rgba-string)))

(defn ->rgba
  [c]
  (cond
    (rgb? c) c
    (hex? c) (hex->rgba c)
    (hsv? c) (hsv->rgba c)
    (lab? c) (throw (ex-info "No implementation of LAB->RGBA available" {:color c}))
    :else (throw (ex-info "Unrecognized color format" {:color c}))))

;;; Formatters

(defn rgba-string
  ([r g b] (rgba-string r g b 1.0))
  ([r g b a]
   (str "rgba(" r "," g "," b "," a ")")))

(defn hsv-string
  [h s v]
  (str "hsv(" h "," s "," v ")"))

(defn lab-string
  [l a b]
  (str "lab(" l "," a "," b ")"))

(defn hex-string
  [& digits]
  (when (not (#{3 6} (count digits)))
    (throw (ex-info "Must provide either 3 or 6 hex digits." {:digits digits})))
  (apply str "#" digits))

;;; Color Manipulation

(defn intensity
  "Return the intensity for 'color'."
  [color]
  (let [[l _ _] (-> color
                    ->rgba
                    rgba->lab
                    parse-lab)]
    (* 255.0 (/ l 100))))

(defn with-intensity
  "Return the same 'color' with intensity of 'desired-intensity'."
  [color desired-intensity]
  (let [[r g b a] (parse-rgba (->rgba color))
        m (/ desired-intensity (intensity color))]
    (rgba-string (* r m) (* g m) (* b m) a)))

(defn similarity
  "Perform a ciede2000 similarity score calculation between the two colors."
  [c1 c2]
  (- 1.0
     (cie/ciede2000
      (double-array
       (parse-lab
        (if (lab? c1) c1
            (-> c1 ->rgba rgba->lab))))
      (double-array
       (parse-lab
        (if (lab? c2) c2
            (-> c2 ->rgba rgba->lab)))))))

(defn lighten
  "Given color 'c' in either hex or rgb[a] format, lighten it by 'percent' [0 1]
  amount. The percentage added is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (lighten c 0.10))
  ([c percent]

   (cond

     (rgb? c)
     (->> c
          parse-rgba
          (map-indexed
           (fn [idx c]
             (if (= idx 3)
               c
               (add-percent c percent))))
          (apply rgba-string))

     (hex? c) (-> c ->rgba (lighten percent))

     (hsv? c) (-> c ->rgba (lighten percent) rgba->hsv)

     :else nil)))

(defn darken
  "Given color 'c' in either hex or rgb[a] format, darken it by 'percent' [0 1]
  amount. The percentage removed is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (darken c 0.10))
  ([c percent] (lighten c (- percent))))

(defn with-opacity
  "Given color 'c' in either hex or rgb[a] format, set its opacity value to
  'opacity' [0 1]. By default it is set to 50%."
  ([c] (with-opacity c 0.50))
  ([c opacity]
   (let [[r g b _] (parse-rgba (->rgba c))]
     (rgba-string r g b (clamp opacity 0 1.0)))))

;;; Private

(defn- abs
  [x]
  #?(:clj (Math/abs (double x))
     :cljs (js/Math.abs (double x))))

(defn- sqrt
  [x]
  #?(:clj (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn- pow
  [x y]
  #?(:clj (Math/pow x y)
     :cljs (js/Math.pow x y)))

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
  (let [c (->> c st/upper-case (filter hex-character?))
        c (condp = (count c)
            6 c
            3 (let [[r g b] c] [r r g g b b])
            (throw (ex-info "Hex colors must have 3 or 6 digits" {:color c})))]
    (->> c
         (partition 2)
         (map (comp (fn [s]
                   #?(:clj (try (Integer/parseInt s 16)
                                (catch Exception e nil))
                      :cljs (js/parseInt s 16)))
                 (partial st/join ""))))))

(defn- clamp
  [x mn mx]
  (when (and x mn mx)
    (max (min x mx) mn)))

(defn- add-percent
  [c p]
  (int (clamp (+ c (* 255 p)) 0 255)))

(defn- digit?
  [x]
  (string->long (str x)))

(defn- digits
  [x]
  (->> (st/split x #"\,")
       (map #(apply str (filter digit? %)))
       (map string->double)))

(defn- rgb-digits
  [c]
  (let [digits (digits c)]
    (cond
      (= 3 (count digits)) (conj (vec digits) 1.0)
      (= 4 (count digits)) digits
      :else nil)))

(defn- parse-rgba
  [rgb-string]
  (rgb-digits rgb-string))

(defn- parse-hex
  [hex-string]
  (hex-digits hex-string))

(defn- hsv-digits
  [c]
  (let [digits (digits c)]
    (when (= 3 (count digits)) digits)))

(defn- parse-hsv
  [hsv-string]
  (hsv-digits hsv-string))

(defn- lab-digits
  [c]
  (let [digits (digits c)]
    (when (= 3 (count digits)) digits)))

(defn- parse-lab
  [lab-string]
  (lab-digits lab-string))

(defn- euclidean-distance
  ([] 0)
  ([x1 x2]
   (sqrt (pow (- x2 x1) 2)))
  ([x1 y1 x2 y2]
   (sqrt
    (+ (pow (- x2 x1) 2)
       (pow (- y2 y1) 2))))
  ([x1 y1 z1 x2 y2 z2]
   (sqrt
    (+ (pow (- x2 x1) 2)
       (pow (- y2 y1) 2)
       (pow (- z2 z1) 2)))))

(defn magnitude
  [c]
  (euclidean-distance (:r c) (:g c) (:b c) 0 0 0))
