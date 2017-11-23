(ns palette.core
  (:require [clojure.string :as st]

            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format :as gformat])

            [palette.cie :as cie]

            [utilis.types.number :refer [string->long string->double]]))

;;; Declarations

(declare add-percent
         rem-percent

         parse-rgb-string
         parse-hex-string
         parse-hsv-string
         parse-cie-string

         hsv->rgba
         rgba->hsv
         rgba->cie

         clamp

         abs sqrt pow

         magnitude
         euclidean-distance)

;;; Records

(defrecord ColorRGBA [r g b a])
(defrecord ColorHSV [h s v])
(defrecord ColorLAB [l a b])

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

(defn cie?
  [c]
  (boolean
   (if (string? c)
     (re-find #"^cie" c)
     (instance? ColorLAB c))))

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
  ([r g b a]
   (when (and r g b a)
     (ColorRGBA.
      (clamp r 0 255)
      (clamp g 0 255)
      (clamp b 0 255)
      (clamp (or a 255) 0 255)))))

(defn color-rgb
  ([c] (color-rgba c))
  ([r g b] (color-rgba r g b 255)))

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

(defn color-cie
  ([c]
   (cond
     (string? c) (when (cie? c)
                   (when-let [[l a b] (parse-cie-string c)]
                     (color-cie l a b)))
     (instance? ColorHSV c) (color-cie (color-rgb c))
     (instance? ColorRGBA c) (rgba->cie c)
     :else nil))
  ([l a b]
   (when (and l a b)
     (ColorLAB.
      (clamp l 0 100)
      (clamp a -128 128)
      (clamp b -128 128)))))

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

(defn rgba->cie
  [rgba]
  (when-let [{:keys [l a b]} (cie/rgb->lab rgba)]
    (color-cie l a b)))

(defn hsv->rgba
  [hsv]
  (let [{:keys [h s v]} hsv
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
    (cie? c) (color-cie c)
    :else nil))

(defn intensity
  "Return the intensity for 'color'."
  ([color]
   (when-let [color (color-rgb color)]
     (magnitude color))))

(defn with-intensity
  "Return the same 'color' with intensity of 'desired-intensity'."
  [color desired-intensity]
  (when-let [c (color-rgba color)]
    (let [m (/ desired-intensity (intensity c))]
      (color-rgba
       (* (:r c) m)
       (* (:g c) m)
       (* (:b c) m)
       (:a c)))))

(defn similarity
  "Perform a ciede2000 similarity score calculation between the two colors."
  [c1 c2]
  (let [c1-cie (color-cie (color-rgba (color c1)))
        c2-cie (color-cie (color-rgba (color c2)))]
    (- 1.0 (cie/ciede2000 c1-cie c2-cie))))

(comment

  (let [c1 (color "hsv(348,41,44)")
        c2 (color "hsv(344,49,24)")
        c3 (color "hsv(320,11,11)")
        c4 (color "hsv(240,13,9)")

        c5 (color "hsv(339,37,18)")
        c6 (color "hsv(300,6,13)")

        c7 (color "hsv(192,15,13)")
        c8 (color "hsv(200,27,9)")
        c9 (color "hsv(300,18,15)")

        c10 (color "hsv(0,0,100)") ;; white
        c11 (color "hsv(0,0,73)") ;; grey

        c12 (color "hsv(0,0,0)") ;; black
        c13 (color "hsv(140,100,2)") ;; also basically black

        c14 (color "hsv(176,64,10)") ;; black
        c15 (color "hsv(173,46,56)") ;; teal

        sim-fn similarity]

    (clojure.pprint/pprint
     [

      ["c1,c2: very similar: " (sim-fn c1 c2)] ;; very similar
      ["c2,c3: very similar: " (sim-fn c2 c3)] ;; very similar (darkness)
      ["c3,c4: very similar: " (sim-fn c3 c4)] ;; very similar (darkness)
      ["c1,c4: very different: " (sim-fn c1 c4)] ;; very different (hues)
      ["c1,c5: pretty similar: " (sim-fn c1 c5)] ;; pretty similar (brightness)
      ["c5,c6: pretty similar: " (sim-fn c5 c6)] ;; pretty similar (darkness)

      ["c7,c8: very similar: " (sim-fn c7 c8)] ;; very similar (darkness)
      ["c8,c9: very similar: " (sim-fn c8 c9)] ;; very similar (darkness)
      ["c7,c9: very similar: " (sim-fn c7 c9)] ;; very similar (darkness)

      ["c10,c11: pretty different: " (sim-fn c10 c11)] ;; pretty different (white vs. light grey)
      ["c12,c13: very similar: " (sim-fn c12 c13)] ;; very similar (both black)
      ["c14,c15: very different: " (sim-fn c14 c15)] ;; very different

      ["c12,c14: very similar " (sim-fn c12 c14)] ;; very similar (both black)

      ]))

  )

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
       (-> color
           color-rgb
           (lighten percent)
           color-hsv)

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

(defn- cie-digits
  [c]
  (let [digits (->> c
                    (re-seq #"[-+]?\d*\.\d+|\d+")
                    (map string->double))]
    (when (= 3 (count digits)) digits)))

(defn- parse-cie-string
  [cie-string]
  (cie-digits cie-string))

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
