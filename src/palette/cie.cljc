(ns palette.cie
  "Borrowed from https://github.com/glpayson/RGB-to-CIEXY/blob/master/rgb-cie-converter/src/rgb_cie_converter/core.clj")

(defn- pow
  [x y]
  #?(:clj (Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn- normalize-color [c]
  (/ c 255.0))

(defn- enhance-color [c]
  (if (> c 0.04045)
    (pow
     (/ (+ c 0.055) 1.055)
     2.4)
    (/ c 12.92)))

(defn- to-xyz [[r g b]]
  [(+
    (* r 0.649926)
    (* g 0.103455)
    (* b 0.197109))
   (+
    (* r 0.234327)
    (* g 0.743075)
    (* b 0.022598))
   (+
    (* r 0.000000)
    (* g 0.053077)
    (* b 1.035763))])

(defn- to-xy [[x y z]]
  (if (= (+ x y z) 0.0)
    [1 1]
    [(/ x (+ x y z))
     (/ y (+ x y z))]))

(defn rgb-to-cie [r g b]
  (->>
   (map normalize-color [r g b])
   (map enhance-color)
   (to-xyz)
   (to-xy)))
