(ns skipbo.game
  (:use karras.entity
        karras.sugar)
  (:import org.bson.types.ObjectId))

(def *WILD* "Wild") ;;display on wild cards
(def *WC* "W") ;;display on corner of wilds
(def *MAX* 12) ;;play sequences 1-*MAX*
(def *DECKS* 20) ;;number of sets of *MAX*
(def *WILDS* 2) ;;wilds per deck


(defembedded Card
  [:number
   :corner
   :card])

(defembedded Player
  [:name
   :id {:type ObjectId}
   :goal {:type :list :of Card}
   :d1 {:type :list :of Card}
   :d2 {:type :list :of Card}
   :d3 {:type :list :of Card}
   :d4 {:type :list :of Card}
   :hand {:type :list :of Card}])

(defentity Game
  [:owner {:type ObjectId}
   :players {:type :list :of Player}
   :deck {:type :list :of Card}
   :done {:type :list :of Card}
   :D1 {:type :list :of Card}
   :D2 {:type :list :of Card}
   :D3 {:type :list :of Card}
   :D4 {:type :list :of Card}
   :starttime
   :winner {:type Player}])

(defn create-deck
  []
  (let [w *WILDS* ;;wilds per set
        m *MAX* ;;set counts to this number 1-m
        d *DECKS*] ;;number of sets to make
    (shuffle
     (concat
      (for [i (range (* w d))] 
        (ensure-type Card {:number (inc m)
                           :corner *WC*
                           :card   *WILD*}))
      (for [i (range 1 (inc d))
            j (range 1 (inc m))]
        (ensure-type Card {:number j
                           :corner j
                           :card   j}))))))

(defn create-player [name]
  (ensure-type Player {:name name
                       :id (ObjectId.)}))

(defn create-game [owner]
  (ensure-type
   Game
   {:owner (:id owner)
    :players [owner]
    :deck (create-deck)}))

(defn add-player [g player]
  (let [ps (:players g)]
    (assoc g :players (conj ps player))))

(defn deal-goal-piles [players deck]
  (let [g-decks (partition 30 deck)]
    (for [i (range (count players))]
      (assoc (nth players i) :goal (nth g-decks i)))))

(defn deal-hand [g]
  (let [ps (:players g)
        p (first ps)
        hand-count (count (:hand p))
        deal      (- 5 hand-count)
        deck      (:deck g)
        deck-left (subvec (vec deck)
                          deal)
        hand (sort-by :number (concat (:hand p)
                                      (take deal deck)))
        update-p (assoc p :hand hand)]
    (assoc g
      :deck deck-left
      :players (conj (rest ps) update-p))))

(defn change-turn [g]
  (deal-hand
   (assoc g :players (conj (butlast (:players g))
                           (last (:players g))))))

(defn start-game [g]
  (let [ps (shuffle (:players g))
        d  (:deck g)
        ;;take 30 cards per player off deck
        deck-left (subvec (vec d)
                          (* 30 (count ps)))
        players (deal-goal-piles ps d)]
    (change-turn
     (assoc g
       :players players
       :deck deck-left
       :starttime (date)))))

(def *INPLAY* [:D1 :D2 :D3 :D4])

(def *pickups*
  [:d1 :d2 :d3 :d4 :h1 :h2 :h3 :h4 :h5 :goal])

(defn putdowns [from]
  (if (some #(= from %) [:d1 :d2 :d3 :d4 :goal])
    [:D1 :D2 :D3 :D4]
    [:D1 :D2 :D3 :D4 :d1 :d2 :d3 :d4]))

(defn get-card [g p l]
  (case l
        :h1 (nth (:hand p) 0)
        :h2 (nth (:hand p) 1)
        :h3 (nth (:hand p) 2)
        :h4 (nth (:hand p) 3)
        :h5 (nth (:hand p) 4)
        :goal (first (:goal p))
        :d1 (first (:d1 p))
        :d2 (first (:d2 p))
        :d3 (first (:d3 p))
        :d4 (first (:d4 p))
        :D1 (first (:D1 g))
        :D2 (first (:D2 g))
        :D3 (first (:D3 g))
        :D4 (first (:D4 g))
        nil))

(defn recycle [D]
  (when (seq D)
    (if (== *MAX* (:corner (first D)))
      nil
      D)))

(defn cleanup
  "done at end of each move. makes sure no piles go too
   high and that player gets new cards in hand if needed."
  [g]
  (let [D1 (recycle (:D1 g))
        D2 (recycle (:D2 g))
        D3 (recycle (:D3 g))
        D4 (recycle (:D4 g))
        done (if D1 (:done g) (concat (:done g) (:D1 g)))
        done (if D2 done (concat done (:D2 g)))
        done (if D3 done (concat done (:D3 g)))
        done (if D4 done (concat done (:D4 g)))
        done (map #(if (= *WILD* (:card %))
                     (assoc % :corner *WC*)
                     %) done)
        [deck done] (if (< 10 (count (:deck g)))
                      [(:deck g) done]
                      [(concat (:deck g) (shuffle done))
                       nil])
        hand (:hand (first (:players g)))
        winner (first
                (filter #(zero? (count (:goal %)))
                        (:players g)))
        cleaned (assoc g
                  :done done
                  :deck deck
                  :winner winner
                  :D1 D1
                  :D2 D2
                  :D3 D3
                  :D4 D4)]
    (if (empty? hand)
      (deal-hand cleaned)
      cleaned)))

(defn remove-from-hand [hand index]
  (let [h (vec hand)
        h1 (subvec h 0 (dec index))
        h2 (subvec h index)]
    (concat h1 h2)))

(defn update-from [p from]
  (let [hand (:hand p)
        newhand #(remove-from-hand hand %)]
    (case from
          :h1 (assoc p :hand (newhand 1))
          :h2 (assoc p :hand (newhand 2))
          :h3 (assoc p :hand (newhand 3))
          :h4 (assoc p :hand (newhand 4))
          :h5 (assoc p :hand (newhand 5))
          :goal (assoc p :goal (rest (:goal p)))
          :d1 (assoc p :d1 (rest (:d1 p)))
          :d2 (assoc p :d2 (rest (:d2 p)))
          :d3 (assoc p :d3 (rest (:d3 p)))
          :d4 (assoc p :d4 (rest (:d4 p))))))

(defn update-players [g p]
  (assoc g
    :players (conj (rest (:players g)) p)))

(defn update-player [p to f]
  (case to
        :d1 (assoc p :d1 (conj (:d1 p) f))
        :d2 (assoc p :d2 (conj (:d2 p) f))
        :d3 (assoc p :d3 (conj (:d3 p) f))
        :d4 (assoc p :d4 (conj (:d4 p) f))))

(defn in-play [pile play]
  (let [c (if (= *WILD* (:card play))
            (assoc play
              :corner (inc (or (:corner (first pile)) 0)))
            play)]
    (conj pile c)))

(defn update-to [g p to f]
  (let [game (update-players g p)]
    (case to
          :d1 (update-players game (update-player p to f))
          :d2 (update-players game (update-player p to f))
          :d3 (update-players game (update-player p to f))
          :d4 (update-players game (update-player p to f))
          :D1 (assoc game :D1 (in-play (:D1 g) f))
          :D2 (assoc game :D2 (in-play (:D2 g) f))
          :D3 (assoc game :D3 (in-play (:D3 g) f))
          :D4 (assoc game :D4 (in-play (:D4 g) f)))))

(defn consecutive? [t f]
  (or
   (= *WILD* (:card f))
   (== (inc (or (:corner t) 0)) (:corner f))))

(defn valid-move? [g p from to f t]
  (if (and (= (:id p) (:id (first (:players g))))
           (some #(= from %) *pickups*)
           (some #(= to %) (putdowns from)))
    (case to
          (:D1 :D2 :D3 :D4)
          (consecutive? t f)
          true)))

(defn make-move [g p from to]
  (let [player (first (:players g))
        f (get-card g player from)
        t (get-card g player to)]
    (when (valid-move? g p from to f t)
      (let [new-p (update-from player from)
            finalize (update-to g new-p to f)]
        (cleanup
         (if (some #(= to %) [:d1 :d2 :d3 :d4])
           (change-turn finalize)
           finalize))))))
