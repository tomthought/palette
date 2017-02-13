(ns palette.core
  (:require [clojure.string :as st]

            #?(:cljs [goog.string :refer [format]])
            #?(:cljs [goog.string.format :as gformat])

            [utilis.types.number :refer [string->long string->double]]))

;;; Declarations

(declare ensure-alpha
         add-percent
         rem-percent

         ->rgba rgb? hex?

         rgb-digits hex-digits)

;;; Public

(defn decompose
  "Return a vector of the RGBA values that comprise the color 'c'."
  [c]
  (cond

    (rgb? c) (ensure-alpha (rgb-digits c))
    (hex? c) (ensure-alpha (hex-digits c))

    :else nil))

(defn lighten
  "Given color 'c' in either hex or rgb[a] format, lighten it by 'percent' [0 1]
  amount. The percentage added is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (lighten c 0.10))
  ([c percent]
   (when-let [[r g b a] (decompose c)]
     (->rgba
      (add-percent r percent)
      (add-percent g percent)
      (add-percent b percent)
      a))))

(defn darken
  "Given color 'c' in either hex or rgb[a] format, darken it by 'percent' [0 1]
  amount. The percentage removed is calculated (* percent 255). The color's
  opacity is not affected."
  ([c] (darken c 0.10))
  ([c percent]
   (when-let [[r g b a] (decompose c)]
     (->rgba
      (rem-percent r percent)
      (rem-percent g percent)
      (rem-percent b percent)
      a))))

(defn transparent
  "Given color 'c' in either hex or rgb[a] format, set its opacity value to
  'opacity' [0 1]. By default it is set to 50%."
  ([c] (transparent c 0.50))
  ([c opacity]
   (when-let [[r g b _] (decompose c)]
     (->rgba r g b opacity))))

;;; Private

(defn- rgb?
  [c]
  (boolean (and (string? c) (re-find #"^rgb" c))))

(defn- hex?
  [c]
  (boolean (and (string? c) (re-find #"^\#" c))))

(defn- rgb-digits
  [c]
  (let [digits (map string->long (re-seq #"\d+" c))]
    (condp = (count digits)
      3 digits
      4 (conj (vec (take 3 digits))
              (float (last digits)))
      5 (conj (vec (take 3 digits))
              (string->double
               (st/join "." (drop 3 digits)))))))

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
  [color]
  (->> color
       st/upper-case
       (filter hex-character?)
       (partition 2)
       (map (comp (fn [s]
                 #?(:clj (Integer/parseInt s 16)
                    :cljs (js/parseInt s 16)))
               (partial st/join "")))))

(defn- ensure-alpha
  [color]
  (if (= 4 (count color))
    color
    (conj (vec color) 1.0)))

(defn- ->rgba
  [r g b a]
  (format "rgba(%s, %s, %s, %s)" r g b (or a 1.0)))

(defn- add-percent
  [c p]
  (int (max (min (+ c (* 255 p)) 255) 0)))

(defn- rem-percent
  [c p]
  (int (max (min (- c (* 255 p)) 255) 0)))
