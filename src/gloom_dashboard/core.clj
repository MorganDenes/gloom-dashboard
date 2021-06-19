(ns gloom-dashboard.core
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :refer [cube union pi difference minkowski translate mirror intersection hull rotate scale]]
            [gloom-dashboard.util :refer [-cy -cu h nh -trap -ltrap -ttlcu]]))

;; (def hpxp-h 13.7)
;; (def hpxp-w 7.1)
;; (def hpxp-d 0.7)
;; (def dial-d 6.6)

(def specs (let [lengths {:big-card {:height 8.9 :width 6.3}
                          :small-card {:height 6.8 :width 4.4}
                          :box {:height 5.6 :width 2.8}
                          :mat {:height 9.5 :width 14.6}
                          :sheet {:height 14 :width 11.5}
                          :discard {:width 1.6 :depth 8.9}
                          :marker {:diameter 1.2}
                          :stat-token {:width 1.45}
                          :cardboard {:width 0.22}
                          :margin 0.2
                          :bevel 2
                          :wall 0.2
                          :deck {:height 3.0}}
                 base (let [{:keys [discard box big-card stat-token margin wall sheet deck]} lengths]
                        {:base {:x (+ (:width discard) (:width box) (:height big-card) (:width stat-token) (:width discard) (* margin 5) (* wall 6))
                                :y (+ (:width sheet) margin (* wall 2))
                                :z (+ (:height deck) wall)}})
                 cover (let [{:keys [mat]} lengths]
                         {:cover {:x (:width mat)
                                  :y (:height mat)
                                  :z 1
                                  :pic (let [x 0.5 y 0.5]
                                         {:x x
                                          :y y
                                          :width (- 5.66 x)
                                          :height (- 7.5 y)})
                                  :hp (let [x 6.3 y 7.8]
                                        {:x x
                                         :y y
                                         :width (- 13.7 x)
                                         :height (- 9.1 y)})
                                  :title (let [x 5.76 y 0.4]
                                           {:x x
                                            :y y
                                            :width (- 13.9 x)
                                            :height (- 1.8 y)})}})
                 caddy {:caddy {:mod-angle 19}}]
             (conj lengths base cover caddy)))

(defn shape
  [object]
  (let [{:keys[cover wall big-card base]} specs
        bevel 2
        lid-z (+ 0.5 (:z cover))
        lid-y (+ 1 (:y cover))
        lid-clip (-cu [(+ wall 0.05) 8.05 10] [(- (h (:x base)) (h wall)) 0 0])
        well (-ltrap [0.7 1.2 (+ (:height big-card) 0.1) (+ lid-z 0.1)]
                     [(- (nh lid-y) 2.8) 0 (h lid-z)])
        corner-o (-cy bevel 1)
        base-o (cube (- (:x base) (* bevel 2)) (- (:y base) (* bevel 2)) (- (:z base) 1))
        case-o (minkowski corner-o base-o)]
    (difference  (intersection case-o object)
                 lid-clip
                 (mirror [1 0 0] lid-clip)
                 (mirror [0 1 0] lid-clip)
                 well
                 (mirror [1 0 0] well))))

(def base-case
  (let [{:keys [cover wall base]} specs
        bevel 2
        lid-z (+ 0.5 (:z cover))
        corner-o (-cy bevel 1)
        corner-i (-cy (- bevel wall) 1)
        base-o (cube (- (:x base) (* bevel 2)) (- (:y base) (* bevel 2)) (- (:z base) 1))
        base-i (->> (cube (- (:x base) (* bevel 2)) (- (:y base) (* bevel 2)) (- (:z base) 1))
                    (translate [0 0 wall]))
        case-o (minkowski corner-o base-o)
        case-i (minkowski corner-i base-i)
        lid-clip (-cu [wall 8 lid-z] [(- (h (:x base)) (h wall)) 0 (+ (h (:z base)) (h lid-z))])]
    (union (difference case-o case-i)
           lid-clip
           (mirror [1 0 0] lid-clip))))

(def base-walls
  (let
   [{:keys [wall base discard small-card margin stat-token box sheet]} specs
    wall-dis (* wall 1.5)
    l-wall (- (h (:x base)))
    b-wall (- (h (:y base)))
    t-wall (h (:y base))
    dicard-wall-x (+ l-wall wall-dis (:width discard))
    small-wall-y (+ b-wall wall-dis (:width small-card) margin margin)
    box-wall-x (+ dicard-wall-x wall-dis (:width box) margin)
    top-wall-h (- t-wall small-wall-y margin)
    token-wall-x (- (- dicard-wall-x) (+ (:width stat-token) wall-dis margin))
    discard-wall-l (-cu [wall (- (:y base) wall) (:z base)] [dicard-wall-x 0 0])
    middle-wall-y (-cu [(* (- dicard-wall-x) 2) wall (:z base)] [0 small-wall-y 0])
    box-wall (-cu [wall top-wall-h (:z base)] [box-wall-x (+ small-wall-y (h top-wall-h)) 0])
    token-wall (-cu [wall top-wall-h (:z base)] [token-wall-x (+ small-wall-y (h top-wall-h)) 0])
    middle-wall-x (-cu [wall (- (- b-wall small-wall-y)) (:z base)] [0 (- small-wall-y (h (+ (:width small-card) wall-dis margin))) 0])]
    (difference
     (union
      discard-wall-l
      (mirror [-1 0 0] discard-wall-l)
      middle-wall-y
      box-wall
      token-wall
      middle-wall-x)
     (-cu [(+ (:height sheet) 0.3) (+ (:width sheet) 0.2) 2] [0 0 2.45]))))

(def base-holes
  (let [{:keys [wall base discard box margin small-card big-card]} specs
        hole (-cy 1.3 10)
        wall-dis (* wall 1.5)
        l-wall (- (h (:x base)))
        dicard-wall-x (+ l-wall wall-dis (:width discard))
        box-wall-x (+ dicard-wall-x wall-dis (:width box) margin)
        t (h (:y base))
        b (nh (:y base))]
    (union
     (translate [(+ (nh (:height small-card)) (nh wall)) (+ b (h (:width small-card)) wall 0.25) 0] hole)
     (translate [(+ (h (:height small-card)) (h wall)) (+ b (h (:height small-card)) wall 0.25) 0] hole)
     (translate [(+ box-wall-x (h (:height big-card)) 0.3) (- t (+ (h (:width big-card)) 0.4)) 0] hole)
     (translate [(- box-wall-x (+ (h (:width box)) 0.25)) (- t (+ (h (:width big-card)) 0.4)) 0] hole))))

(def lid
  (let [{:keys [cover mat stat-token cardboard base]} specs
        lid-x (+ 1 (:x cover))
        lid-y (+ 1 (:y cover))
        lid-z (+ 0.5 (:z cover))
        mat-x (+ (:width mat) 0.05)
        mat-y (+ (:height mat) 0.1)
        window-x (- (:width mat) 0.4)
        window-y (- (:height mat) 0.4)
        token-slot (-cu [(+ (:width stat-token) 0.05) (+ (:width cardboard) 0.05) 1] [(+ (nh (:x base)) 2) (- (h (:y base)) 0.7) (+ (:z cover) 0.7)])]
    (difference
     (-cu [(:x base) (:y base) lid-z] [0 0 (h lid-z)])
     (-cu [window-x window-y 5] [0 0 0])
     (-cu [(+ mat-x 0.05) (+ mat-y 0.05) 1] [0 0 0.65])
     token-slot
     (translate [2 0 0] token-slot)
     (translate [4 0 0] token-slot)
     (translate [6 0 0] token-slot)
     (rotate (/ pi 2) [0 0 1] (-trap [(+ (:y cover) 0.05) (+ (:y cover) 0.55) (+ (:x base) 0.05) (+ (:z cover) 0.05)] [0 0 (+ 0.5 (h (:z cover)))])))))

(defn card-caddy
  [base location]
  (let [{:keys [small-card margin cover wall caddy]} specs
        {:keys [title hp]} cover
        caddy-w (+ (:height small-card) margin)
        caddy-h (- (+ (:y hp) (:height hp)) (:y title))
        [caddy-x caddy-y] location
        base-h 1.25
        mod-base (-cu [(+ caddy-w margin) wall base-h] [0 (h (+ caddy-h wall)) (h base-h)])
        cut-box (translate [0 (- (h caddy-h)) 0]
                           (rotate (* (:mod-angle caddy) (/ pi 180)) [1 0 0]
                                   (-cu [caddy-w 20 0.68] [0 10 0.34])))
        card-base (->> (-cu [caddy-w (+ (:width small-card)) 0.5] [0 (h (+ (:width small-card))) -0.25])
                       (rotate (* (:mod-angle caddy) (/ pi 180)) [1 0 0])
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
     (move (-cu [(h (:height small-card)) (- caddy-h 0.1) 5] [0 0 2.6]))
     (move (-cu [(+ caddy-w 0.1) wall 5] [0 0 2.6])))))

(def top-cover
  (let [{:keys [cover base]} specs
        {:keys [pic title hp]} cover
        t (h (:y cover))
        l (nh (:x cover))
        pic-shape (-ttlcu [(:width pic) (:height pic) 5] [(+ l (:x pic)) (- t (:y pic)) 2])
        title-shape (-ttlcu [(:width title) (:height title) 5] [(+ l (:x title)) (- t (:y title)) 2])
        hp-shape (-ttlcu [(:width hp) (:height hp) 5] [(+ l (:x hp)) (- t (:y hp)) 2])]
    (difference
     (card-caddy
      (rotate (/ pi 2) [0 0 1] (-trap [(:y cover) (+ (:y cover) 0.5) (:x base) (:z cover)] [0 0 (h (:z cover))]))
      ;; (-cu [cover-x cover-y cover-z] [0 0 (h cover-z)])
      [(+ (nh (:x cover)) (+ (:x hp) (h (:width hp))))
       (- (- (h (:y cover)) (:y title)) (h (- (+ (:y hp) (:height hp)) (:y title))))])
     pic-shape title-shape hp-shape)))

(defn -main
  "I don't do a whole lot."
  []
  (spit "things/test.scad"
        (let
         [{:keys [base]} specs
          bottom (difference (union base-case base-walls) base-holes)]
          ;; cy (difference  (->> (-cy 12 7.7)
          ;;                      (rotate (/ pi 2) [0 1 0])
          ;;                      (translate [0 0 -11])) (-cu [50 50 50] [0 0 -25]))]
          (write-scad (scale [10 10 10] ;;(difference bottom (->> (cube 50 50 50)
                                               ;;(translate [0 0 -23.5])))

                            ;;  (-cu [(+ stat-token 0.2) (+ cardboard-w 0.2) 1] [(+ (nh base-x) 2) (- (h base-y) 0.5) (+ cover-z 0.1)])
                            ;;  bottom
                            ;;  (-cu [14 wall (+ 1 cover-z)] [0 (- (h base-y) (h wall)) (+ (h base-z) (h (+ 0.5 cover-z)))])
                            ;;  (card-caddy cy [0 0])
                            ;;  top-cover
                             (translate [0 0 (nh (:z base))] bottom)
                             (shape lid)
                             (shape (translate [0 0 0.5] top-cover))
                             )))))
