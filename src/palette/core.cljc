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
         euclidean-distance

         visual-similarity-score)

;;; Records

(defrecord ColorRGBA [r g b a])
(defrecord ColorHSV [h s v])
(defrecord ColorCIE [x y])

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
     (instance? ColorCIE c))))

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
                   (when-let [[x y] (parse-cie-string c)]
                     (color-cie x y)))
     (instance? ColorHSV c) (color-cie (color-rgb c))
     (instance? ColorRGBA c) (rgba->cie c)
     :else nil))
  ([x y]
   (when (and x y)
     (ColorCIE.
      (clamp x 0 1)
      (clamp y 0 1)))))

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
  (let [{:keys [r g b]} rgba]
    (when-let [[x y] (cie/rgb-to-cie r g b)]
      (color-cie x y))))

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
  "Perform a euclidean distance calculation between the two colors."
  [c1 c2]
  (let [c1-cie (color-cie (color-rgb (color c1)))
        c2-cie (color-cie (color-rgb (color c2)))]
    (- 1.0 (euclidean-distance
            (:x c1-cie)
            (:y c1-cie)
            (:x c2-cie)
            (:y c2-cie)))))

(defn visual-similarity
  "Measure the similarity between two colors. Similarity is between 0 and 1.
  Note that this function is attempting to measure how similar two colors are to
  the human eye. So colors are that are similar in hue might score very well,
  even if their RGB values would not indicate this."
  [c1 c2]
  (float

   (let [c1-rgb (color-rgb (color c1))
         c2-rgb (color-rgb (color c2))]

     (or

      ;; When RGB is not similar, fallback to check if HSV is similar.
      (let [c1 (color-hsv c1-rgb)
            c2 (color-hsv c2-rgb)

            c1-dark-or-not-saturated? (or (< (:v c1) 20) (< (:s c1) 10))
            c2-dark-or-not-saturated? (or (< (:v c2) 20) (< (:s c2) 10))]
        (when (and c1 c2)

          (cond

            ;; both are dark or not saturated
            (and c1-dark-or-not-saturated? c2-dark-or-not-saturated?)
            (visual-similarity-score
             (:h c1) (:s c1) (:v c1)
             (:h c2) (:s c2) (:v c2)
             360 40 40
             0.0 0.25 0.75)

            ;; one of them is dark or not saturated
            (or c1-dark-or-not-saturated? c2-dark-or-not-saturated?)
            (visual-similarity-score
             (:h c1) (:s c1) (:v c1)
             (:h c2) (:s c2) (:v c2)
             360 100 20
             0.10 0.20 0.70)

            ;; perform normal score
            :else (visual-similarity-score
                   (:h c1) (:s c1) (:v c1)
                   (:h c2) (:s c2) (:v c2)
                   60 100 100
                   0.60 0.30 0.10))))
      0))))

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

        sim-fn visual-similarity]

    (clojure.pprint/pprint
     [

      ["c1,c2: very similar: " (sim-fn c1 c2)] ;; very similar
      ["c2,c3: pretty different: " (sim-fn c2 c3)] ;; pretty different (darkness)
      ["c3,c4: very similar: " (sim-fn c3 c4)] ;; very similar (darkness)
      ["c1,c4: very different: " (sim-fn c1 c4)] ;; very different (hues)
      ["c1,c5: very different: " (sim-fn c1 c5)] ;; very different (brightness)
      ["c5,c6: pretty similar: " (sim-fn c5 c6)] ;; pretty similar (darkness)

      ["c7,c8: very similar: " (sim-fn c7 c8)] ;; very similar (darkness)
      ["c8,c9: very similar: " (sim-fn c8 c9)] ;; very similar (darkness)
      ["c7,c9: very similar: " (sim-fn c7 c9)] ;; very similar (darkness)

      ["c10,c11: pretty different: " (sim-fn c10 c11)] ;; pretty different (white vs. light grey)

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
    (when (= 2 (count digits)) digits)))

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

(defn- visual-similarity-score
  [h1 s1 v1 h2 s2 v2 ht st vt hw sw vw]
  (let [h-zero-threshold ht
        s-zero-threshold st
        v-zero-threshold vt

        h-diff (clamp (abs (- h1 h2)) 0 h-zero-threshold)
        s-diff (clamp (abs (- s1 s2)) 0 s-zero-threshold)
        v-diff (clamp (abs (- v1 v2)) 0 v-zero-threshold)

        h-diff-normalized (/ h-diff h-zero-threshold)
        s-diff-normalized (/ s-diff s-zero-threshold)
        v-diff-normalized (/ v-diff v-zero-threshold)

        h-weight hw
        s-weight sw
        v-weight vw]

    (+ (* h-weight (- 1.0 h-diff-normalized))
       (* s-weight (- 1.0 s-diff-normalized))
       (* v-weight (- 1.0 v-diff-normalized)))))
