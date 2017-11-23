(ns palette.cie)

;;; Declarations

(def sqrt #?(:cljs js/Math.sqrt
             :clj #(Math/sqrt (double %))))
(def pow #?(:cljs js/Math.pow
            :clj #(Math/pow (double %1)
                            (double %2))))
(def cos #?(:cljs js/Math.cos
            :clj #(Math/cos (double %))))
(def atan2 #?(:cljs js/Math.atan2
              :clj #(Math/atan2 (double %1) (double %2))))
(def sin #?(:cljs js/Math.sin
            :clj #(Math/sin %)))
(def abs #?(:cljs js/Math.abs
            :clj #(Math/abs (double %))))
(def exp #?(:cljs js/Math.exp
            :clj #(Math/exp (double %))))
(def PI #?(:cljs js/Math.PI
           :clj Math/PI))

(declare hp_f dhp_f a_hp_f degrees radians

         rgb->xyz
         xyz->lab)

;;; API

(defn ciede2000
  [c1 c2]

  (let [l1 (:l c1)
        a1 (:a c1)
        b1 (:b c1)

        l2 (:l c2)
        a2 (:a c2)
        b2 (:b c2)

        _ (when-not (and l1 a1 b1 l2 a2 b2)
            (throw
             (ex-info
              "Colors provided to ciede2000 must be in LAB color space."
              {:c1 c1
               :c2 c2})))

        kl 1
        kc 1
        kh 1

        ;; Calculate C1p, C2p, h1p, h2p
        c1* (sqrt (+ (pow a1 2) (pow b1 2)))
        c2* (sqrt (+ (pow a2 2) (pow b2 2)))

        a_c1_c2 (/ (+ c1* c2*) 2.0)

        g (* 0.50
             (- 1.0
                (sqrt
                 (+ (/ (pow a_c1_c2 7.0)
                       (+ (pow a_c1_c2 7.0)
                          (pow 25.0 7.0)))))))

        a1p (* (+ 1.0 g) a1)
        a2p (* (+ 1.0 g) a2)

        c1p (sqrt (+ (pow a1p 2) (pow b1 2)))
        c2p (sqrt (+ (pow a2p 2) (pow b2 2)))

        h1p (hp_f b1 a1p)
        h2p (hp_f b2 a2p)

        dLp (- l2 l1)
        dCp (- c2p c1p)

        dhp (dhp_f c1* c2* h1p h2p)
        dHp (* 2 (sqrt (* c1p c2p)) (sin (/ (radians dhp) 2.0)))

        a_L (/ (+ l1 l2) 2.0)
        a_Cp (/ (+ c1p c2p) 2.0)
        a_hp (a_hp_f c1* c2* h1p h2p)

        t (- (+ (- 1.0 (* 0.17 (cos (radians (- a_hp 30)))))
                (* 0.24 (cos (radians (* 2 a_hp))))
                (* 0.32 (cos (radians (+ (* 3 a_hp) 6)))))
             (* 0.20 (cos (radians (- (* 4 a_hp) 63)))))

        d_ro (* 30 (exp (- (pow (/ (- a_hp 275) 25.0) 2))))

        rc (sqrt (/ (pow a_Cp 7.0)
                    (+ (pow a_Cp 7.0)
                       (pow 25.0 7.0))))

        sl (+ 1 (/ (* 0.015 (pow (- a_L 50) 2))
                   (sqrt (+ 20 (pow (- a_L 50) 2)))))

        sc (+ 1.0 (* 0.045 a_Cp))
        sh (+ 1.0 (* 0.015 a_Cp t))
        rt (* -2 rc (sin (radians (* 2 d_ro))))
        de (sqrt (+ (pow (/ dLp (* sl kl)) 2)
                    (pow (/ dCp (* sc kc)) 2)
                    (pow (/ dHp (* sh kh)) 2)
                    (* rt
                       (/ dCp (* sc kc))
                       (/ dHp (* sh kh)))))]
    (max (min (/ de 100) 1.0) 0.0)))

(defn rgb->lab
  [c]
  (xyz->lab (rgb->xyz c)))

(defn rgba->lab
  [c]
  (rgb->lab c))

;;; Private

(defn- degrees
  [n]
  (* n (/ 180 PI)))

(defn- radians
  [n]
  (* n (/ PI 180)))

(defn- hp_f
  [x y]
  (if (and (zero? x) (zero? y))
    0
    (let [tmphp (degrees (atan2 x y))]
      (if (>= tmphp 0)
        tmphp
        (+ tmphp 360)))))

(defn- dhp_f
  [c1 c2 h1p h2p]
  (let [hdiff (- h2p h1p)]
    (cond
      (zero? (* c1 c2)) 0
      (<= (abs hdiff) 180) hdiff
      (> hdiff 180) (- hdiff 360)
      (< hdiff -180) (+ hdiff 360)
      :else (throw
             (ex-info
              "Unknown condition."
              {:c1 c1
               :c2 c2
               :h1p h1p
               :h2p h2p})))))

(defn- a_hp_f
  [c1 c2 h1p h2p]
  (let [hdiff (- h1p h2p)
        hsum (+ h1p h2p)]
    (cond
      (zero? (* c1 c2)) hsum
      (<= (abs hdiff) 180) (/ hsum 2.0)
      (and (> (abs hdiff) 180) (< hsum 360)) (/ (+ hsum 360) 2.0)
      (and (> (abs hdiff) 180) (>= hsum 360)) (/ (- hsum 360) 2.0)
      :else (throw
             (ex-info
              "Unknown condition."
              {:c1 c1
               :c2 c2
               :h1p h1p
               :h2p h2p})))))

(defn- rgb->xyz
  [c]
  (let [{:keys [r g b]} c
        r (/ r 255)
        g (/ g 255)
        b (/ b 255)

        adjust #(* 100 (if (> % 0.04045)
                         (pow (/ (+ % 0.055) 1.055) 2.4)
                         (/ % 12.92)))

        r (adjust r)
        g (adjust g)
        b (adjust b)]

    {:x (+ (* r 0.4124) (* g 0.3576) (* b 0.1805))
     :y (+ (* r 0.2126) (* g 0.7152) (* b 0.0722))
     :z (+ (* r 0.0193) (* g 0.1192) (* b 0.9505))}))

(defn- xyz->lab
  [c]
  (let [{:keys [x y z]} c

        ref-y 100.000
        ref-z 108.883
        ref-x 95.047

        y (/ y ref-y)
        z (/ z ref-z)
        x (/ x ref-x)

        adjust #(if (> % 0.008856)
                  (pow % (/ 1 3))
                  (+ (* 7.787 %)
                     (/ 16 116)))

        x (adjust x)
        y (adjust y)
        z (adjust z)

        l (- (* 116 y) 16)
        a (* 500 (- x y))
        b (* 200 (- y z))]

    {:l l :a a :b b}))

(defn test-cie
  []
  (let [dark-grey (rgb->lab {:r 10 :g 10 :b 10})
        light-grey (rgb->lab {:r 100 :g 100 :b 100})

        white (rgb->lab {:r 255 :g 255 :b 255})
        black (rgb->lab {:r 0 :g 0 :b 0})]

    [[:dark-grey/light-grey (ciede2000 dark-grey light-grey)]
     [:black/white (ciede2000 black white)]]

    ))
