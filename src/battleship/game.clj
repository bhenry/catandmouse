(ns battleship.game
  (:use karras.entity
        karras.sugar)
  (:import org.bson.types.ObjectId))

(defn init-board [x]
  (vec (repeat x (vec (repeat x nil)))))

(defn board-space [b x y]
  (get-in b [y x]))

(defn get-bearing [boat]
  (let [{[ax ay] :a [bx by] :b} boat]
    (cond (< ax bx) :east
          (< ay by) :south
          (> ax bx) :west
          (> ay by) :north)))

(defn boat-class [boat [x y]]
  (let [d (get-bearing boat) ;;direction
        p (cond (= (:a boat) [x y]) :back
                (= (:b boat) [x y]) :front
                :else :middle)]
    (keyword (str (name d) (name p)))))

(defn space-class [b x y]
  (let [space (board-space b x y)]
    (cond (= space :miss)
          "splash"
          (nil? space)
          "empty"
          :else
          (boat-class space [x y]))))
