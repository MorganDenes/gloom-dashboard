(ns gloom-dashboard.core
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))
            ;; [unicode-math.core :refer :all]))z

(def big-h 8.9)
(def big-w 6.3)
(def small-h 6.8)
(def small-w 4.4)
(def box-h 5.6)
(def box-w 2.8)
(def mat-h 9.5)
(def mat-w 14.6)
;; (def hpxp-h 13.7)
;; (def hpxp-w 7.1)
;; (def hpxp-d 0.7)
(def dial-d 6.6)
(def sheet-h 14)
(def sheet-w 11.5)
(def discard-w 1.6)
(def discard-d 8.9)

(def token 1.2)
(def stat-token 1.45)

(def margin 0.2)
(def bevel 2)
(def wall 0.2)
(def deck-h 3)

(def base-x (+ discard-w box-w big-h token discard-w (* margin 5) (* wall 6)))
(def base-y (+ sheet-w margin (* wall 2)))
(def base-z (+ deck-h wall))

(def cover-x (+ mat-w))
(def cover-y (+ mat-h))
(def cover-z 1)

(def pic-x 0.5)
(def pic-y 0.5)
(def pic-w (- 5.66 pic-x))
(def pic-h (- 7.5 pic-y))

(def hp-x 6.3)
(def hp-y 7.8)
(def hp-w (- 13.7 hp-x))
(def hp-h (- 9.1 hp-y))

(def title-x 5.76)
(def title-y 0.4)
(def title-h (- 1.8 title-y))
(def title-w (- 13.9 title-x))

(def mod-angle 19)
(def cardboard-w 0.22)

(defn h "Half the value" [v] (/ v 2))
(defn nh "Negative half value" [v] (- (h v)))

(defn -cy
  "Create a cylinder with a higher resolution"
  [r h]
  (->> (cylinder (* r 5) h)
       (scale [(/ 1 5) (/ 1 5) 1])))

(defn -cu
  [size location]
  (let [[x y z] size]
    (->> (cube x y z)
         (translate location))))

(defn -ttlcu
  [size location]
  (let [[x y z] size]
    (translate [(h x) (nh y) (nh z)] (-cu size location))))

(defn -ltrap
  [size location]
  (let [[x1 x2 y z] size
        [xx yy zz] location
        inter (cube x2 y z)]
    (translate [xx yy zz]
               (intersection
                inter
                (hull (-cu [x1 y z] [(nh (- x2 x1)) 0 0])
                      (-cu [x2 y z] [0 0 (- z)]))))))


(defn -trap
  [size location]
  (let [[x1 x2 y z] size
        [xx yy zz] location
        inter (cube x2 y z)]
    (translate [xx yy zz] (intersection
      inter
      (hull (cube x1 y z)
            (-cu [x2 y z] [0 0 (- z)]))))))

(defn shape [object]
  (let [bevel 2
        lid-z (+ 0.5 cover-z)
        lid-y (+ 1 cover-y)
        lid-clip (-cu [(+ wall 0.05) 8.05 10] [(- (h base-x) (h wall)) 0 0])
        well (-ltrap [0.7 1.2 (+ big-h 0.1) (+ lid-z 0.1)]
                     [(- (nh lid-y) 2.8) 0 (h lid-z)])
        corner-o (-cy bevel 1)
        base-o (cube (- base-x (* bevel 2)) (- base-y (* bevel 2)) (- base-z 1))
        case-o (minkowski corner-o base-o)]
  (difference  (intersection case-o object)
               lid-clip
               (mirror [1 0 0] lid-clip)
               (mirror [0 1 0] lid-clip)
               well
               (mirror [1 0 0] well))))

(def base
  (let [bevel 2
        lid-z (+ 0.5 cover-z)
        corner-o (-cy bevel 1)
        corner-i (-cy (- bevel wall) 1)
        base-o (cube (- base-x (* bevel 2)) (- base-y (* bevel 2)) (- base-z 1))
        base-i (->> (cube (- base-x (* bevel 2)) (- base-y (* bevel 2)) (- base-z 1))
                    (translate [0 0 wall]))
        case-o (minkowski corner-o base-o)
        case-i (minkowski corner-i base-i)
        lid-clip (-cu [wall 8 lid-z] [(- (h base-x) (h wall)) 0 (+ (h base-z) (h lid-z))])]
    (union (difference case-o case-i)
           lid-clip
           (mirror [1 0 0] lid-clip))))

(def base-walls
  (let
   [wall-dis (* wall 1.5)
    l-wall (- (h base-x))
    b-wall (- (h base-y))
    t-wall (h base-y)
    dicard-wall-x (+ l-wall wall-dis discard-w)
    small-wall-y (+ b-wall wall-dis small-w margin margin)
    box-wall-x (+ dicard-wall-x wall-dis box-w margin)
    top-wall-h (- t-wall small-wall-y margin)
    token-wall-x (- (- dicard-wall-x) (+ token wall-dis margin))
    discard-wall-l (-cu [wall (- base-y wall) base-z] [dicard-wall-x 0 0])
    middle-wall-y (-cu [(* (- dicard-wall-x) 2) wall base-z] [0 small-wall-y 0])
    box-wall (-cu [wall top-wall-h base-z] [box-wall-x (+ small-wall-y (h top-wall-h)) 0])
    token-wall (-cu [wall top-wall-h base-z] [token-wall-x (+ small-wall-y (h top-wall-h)) 0])
    middle-wall-x (-cu [wall (- (- b-wall small-wall-y)) base-z] [0 (- small-wall-y (h (+ small-w wall-dis margin))) 0])]
    (difference
     (union
      discard-wall-l
      (mirror [-1 0 0] discard-wall-l)
      middle-wall-y
      box-wall
      token-wall
      middle-wall-x)
     (-cu [(+ sheet-h 0.3) (+ sheet-w 0.2) 2] [0 0 2.45]))))

(def base-holes
  (let [hole (-cy 1.3 10)
        wall-dis (* wall 1.5)
        l-wall (- (h base-x))
        dicard-wall-x (+ l-wall wall-dis discard-w)
        box-wall-x (+ dicard-wall-x wall-dis box-w margin)
        t (h base-y)
        b (nh base-y)]
    (union
     (translate [(+ (nh small-h) (nh wall)) (+ b (h small-w) wall 0.25) 0] hole)
     (translate [(+ (h small-h) (h wall)) (+ b (h small-w) wall 0.25) 0] hole)
     (translate [(+ box-wall-x (h big-h) 0.3) (- t (+ (h big-w) 0.4)) 0] hole)
     (translate [(- box-wall-x (+ (h box-w) 0.25)) (- t (+ (h big-w) 0.4)) 0] hole)
     )))

(def lid
  (let [lid-x (+ 1 cover-x)
        lid-y (+ 1 cover-y)
        lid-z (+ 0.5 cover-z)
        mat-x (+ mat-w 0.05)
        mat-y (+ mat-h 0.1)
        window-x (- mat-w 0.4)
        window-y (- mat-h 0.4)
        token-slot (-cu [(+ stat-token 0.05) (+ cardboard-w 0.05) 1] [(+ (nh base-x) 2) (- (h base-y) 0.7) (+ cover-z 0.7)])]
    (difference
     (-cu [base-x base-y lid-z] [0 0 (h lid-z)])
     (-cu [window-x window-y 5] [0 0 0])
     (-cu [(+ mat-x 0.05) (+ mat-y 0.05) 1] [0 0 0.65])
     token-slot
     (translate [2 0 0] token-slot)
     (translate [4 0 0] token-slot)
     (translate [6 0 0] token-slot)
     (rotate (/ pi 2) [0 0 1] (-trap [(+ cover-y 0.05) (+ cover-y 0.55) (+ base-x 0.05) (+ cover-z 0.05)] [0 0 (+ 0.5 (h cover-z))]))))
  )

(defn card-caddy
  [base location]
  (let [caddy-w (+ small-h margin)
        caddy-h (- (+ hp-y hp-h) title-y)
        [caddy-x caddy-y] location
        base-h 1.25
        mod-base (-cu [(+ caddy-w margin) wall base-h] [0 (h (+ caddy-h wall)) (h base-h)])
        cut-box (translate [0 (- (h caddy-h)) 0]
                           (rotate (* mod-angle (/ pi 180)) [1 0 0]
                                   (-cu [caddy-w 20 0.68] [0 10 0.34])))
        card-base (->> (-cu [caddy-w (+ small-w) 0.5] [0 (h (+ small-w)) -0.25])
                       (rotate (* mod-angle (/ pi 180)) [1 0 0])
                       (translate [0 (nh caddy-h) 0]))
        move (fn [x] (translate [caddy-x caddy-y 0] x))]
    (difference
     (union (difference
             base
             (move cut-box)
             (move (mirror [0 1 0] cut-box)))
            (move mod-base)
            (move (mirror [0 1 0] mod-base))
            (move (difference (hull card-base
                                    (mirror [0 1 0] card-base))
                              (-cu [10 10 5] [0 0 -2.5]))))
     (move (-cu [(h small-h) (- caddy-h 0.1) 5] [0 0 2.6]))
     (move (-cu [(+ caddy-w 0.1) wall 5] [0 0 2.6])))
    ))

(def top-cover
  (let [t (h cover-y)
        l (nh cover-x)
        pic (-ttlcu [pic-w pic-h 5] [(+ l pic-x) (- t pic-y) 2])
        title (-ttlcu [title-w title-h 5] [(+ l title-x) (- t title-y) 2])
        hp (-ttlcu [hp-w hp-h 5] [(+ l hp-x) (- t hp-y) 2])]
     (difference
    (card-caddy
     (rotate (/ pi 2) [0 0 1] (-trap [cover-y (+ cover-y 0.5) base-x cover-z] [0 0 (h cover-z)]))
      ;; (-cu [cover-x cover-y cover-z] [0 0 (h cover-z)])
     [(+ (nh cover-x) (+ hp-x (h hp-w)))
     (- (- (h cover-y) title-y) (h (- (+ hp-y hp-h) title-y)))])
      pic title hp
      )
    
    ))

(defn -main
  "I don't do a whole lot."
  []
  (spit "things/test.scad"
        (let
         [bottom (difference (union base base-walls) base-holes)
          cy (difference  (->> (-cy 12 7.7)
                               (rotate (/ pi 2) [0 1 0])
                               (translate [0 0 -11])
                               ) (-cu [50 50 50] [0 0 -25]))]
          (write-scad (scale [10 10 10] ;;(difference bottom (->> (cube 50 50 50)
                                               ;;(translate [0 0 -23.5])))

                            ;;  (-cu [(+ stat-token 0.2) (+ cardboard-w 0.2) 1] [(+ (nh base-x) 2) (- (h base-y) 0.5) (+ cover-z 0.1)])
                            ;;  bottom
                            ;;  (-cu [14 wall (+ 1 cover-z)] [0 (- (h base-y) (h wall)) (+ (h base-z) (h (+ 0.5 cover-z)))])
                            ;;  (card-caddy cy [0 0])
                            ;;  top-cover
                            (translate [0 0 (nh base-z)] bottom)
                            ;;  (shape lid)
                            ;;  (shape (translate [0 0 0.5] top-cover))
                             )))))
