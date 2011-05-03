(ns battleship.views
  (:use [hiccup.core]
        [clojure.contrib.str-utils :only [re-gsub]]
        [hiccup.page-helpers]
        [karras.entity]
        [battleship.game])
  (:import [battleship.game Game]
           [org.bson.types ObjectId]))

(defn redirect-to
  "A shortcut for a '302 Moved' HTTP redirect."
  [location]
  {:status 302 :headers {"Location" location}})

(defn base
  "takes a context map to fill in a base template
   :messages => vector of strings.

   :scripts => any extra stuff you want to add as the last thing
    before body

   :extrahead => any extra stuff you want to add as the last thing
    in <head>"
  [c]
  (let [defaults {:status 200
                  :headers {"Content-Type" "text/html"}
                  :pagetitle "Cat and Mouse"
                  :banner (html
                           [:a {:href "http://en.wikipedia.org/wiki/Spite_and_Malice"
                                :target "_blank"
                                :title "how to play"}
                            "Cat and Mouse"])}
        context (merge defaults c)]
    {:status (context :status)
     :headers (context :headers)
     :body (html
            [:html
             [:head
              [:title (:pagetitle context)]
              (include-js "/media/jquery.js")
              (include-js "/media/scripts.js")
              (include-css "/media/style.css")
              (:extrahead context)]
             [:body
              [:div#container
               [:div#top
                [:h1#title (:banner context)]
                [:div#menu]]
               [:div#middle
                (when (:messages context)
                  [:div.messages (:messages context)])
                (when (:pagename context)
                  [:h2#pagename (:pagename context)])
                [:div#content (:content context)]]
               [:div#bottom
                [:p "&copy;2011 "
                 [:a {:href "http://brandonhenry.net/"
                      :target "_blank"}
                  "brandonhenry.net"]]]]
              (:scripts context)]])}))
;;;;=========
;;==== basics
;;;;=========

(defn not-found [req]
  (base {:status 404
                   :pagetitle "Page Not Found"
                   :pagename "Page Not Found"
                   :content (list [:h2 "Sorry. We could not locate the page:"]
                                  [:h3 (req :uri)]
                                  [:div ""])}))

(defn server-error [req]
  (base
   {:status 500
    :pagetitle "Page Error"
    :pagename "Page Error"
    :content "Not Found"}))

(defn get-player-by-id [g p]
  (first (filter #(= (ObjectId. p) (:id %))
                 (:players g))))

(defn get-owner [g]
  (get-player-by-id g (str (:owner g))))

(defn owner? [g p]
  (= (:id p) (:owner g)))

(defn new [req]
  (let [name (get (:params req) "name")]
    (if name
      (let [owner (create-player name)
            game (save (create-game owner))]
        (redirect-to (format "/wait?g=%s&p=%s"
                             (:_id game)
                             (:id owner))))
      (base
       {:pagetitle "Cat and Mouse"
        :pagename "Start a new game."
        :content [:div
                  [:form {:method "POST"}
                   "Type your name and click the
                    button to invite up to 5 players"
                   [:br]
                   [:input {:type "text"
                            :name "name"}]
                   [:input {:type "submit"
                            :value "Invite Others"}]]]}))))

(defn wait [req]
  (let [g (get (:params req) "g")
        p (get (:params req) "p")
        game (fetch-by-id Game g)
        player (get-player-by-id game p)
        players (:players game)
        join-link (format "http://%s/join?g=%s"
                          (get (:headers req) "host")
                          g)
        own? (owner? game player)
        button (get (:params req) "start")
        redir (redirect-to (format
                            "/game?g=%s&p=%s" g p))]
    (cond (and own?
               (< 1 (count players))
               button)
          (do (save (start-game game))
              redir)
          (:starttime game) redir
          :default-view
          (base
           {:pagetitle "Cat and Mouse"
            :pagename "Awaiting Players"
            :extrahead [:meta {:HTTP-EQUIV "REFRESH"
                               :CONTENT "5"}]
            :messages (when button "You can't start
                                    a game by yourself.")
            :content [:div
                      (if own?
                        [:div.invite
                         "Invite up to 5 friends to play
                        by sending this link."
                         [:br]
                         [:input {:type "text"
                                  :size 75
                                  :value join-link}]])
                      [:div.players
                       [:h2 "Players"]
                       [:ol
                        (for [pl (reverse players)]
                          [:li.player
                           [:h3.playername (:name pl)]])]]
                      (if own?
                        [:div
                         [:form {:method "POST"}
                          [:input {:type "submit"
                                   :name "start"
                                   :value "Start Game"}]]])]}))))

(defn join [req]
  (let [g (get (:params req) "g")
        game (fetch-by-id Game g)
        owner (get-owner game)
        name (get (:params req) "name")]
    (if (and name
             (< (count (:players game)) 6)
             (nil? (:starttime game)))
      (let [player (create-player name)
            p (:id player)
            redir (redirect-to (format
                                "/wait?g=%s&p=%s" g p))]
        (save (add-player game player))
        redir)
      (base
       {:pagetitle "Cat and Mouse"
        :pagename "Join a game"
        :content [:div
                  (if name
                    (format "Sorry %s. This game is full
                             or already started.
                             <a href=\"/\">
                               Start your own here.</a>"
                            name)
                    (format
                     "%s has invited you to play a game
                      of Cat and Mouse. Type your name
                      and click to join in."
                     (:name owner)))
                  [:div.form
                   [:form {:method "POST"}
                    [:input {:type "text"
                             :name "name"}]
                    [:input {:type "submit"
                             :value "Join Game"}]]]]}))))

(defn display-card
  "playable and dropzone are position codes
   stack is the number of cards in the pile"
  [c & [faceup? playable dropzone stack discard]]
  (if faceup?
    [:div.card
     [:div.corner (:corner c)
       (for [x (take 5 discard)]
         [:span.under " " (:corner x)])]
     [:div.middle (:card c)]
     (if (and c playable)
       (let [id (ObjectId.)]
         [:div.bottom
          [:input {:id id
                   :type "radio"
                   :name "playme"
                   :value playable}]
          [:label {:for id} "Pick"]]))
     (if dropzone
       [:div.dropbutton
        [:input {:type "submit"
                 :name dropzone
                 :value "Drop"}]])
     (if stack
       [:div.stackcount
        "(" stack ")"])]
    [:div.back "&nbsp;"]))

(defn playerview [player hand goal yours]
  [(if yours :div.ownzone :div.playerzone)
   [:h3 (:name player)]
   [:div.hand
    (for [i (range (count hand))]
      (display-card (nth hand i)
                    yours
                    (when yours (str "h" (inc i)))))]
   [:hr {:style "clear:both;"}]
   [:div.stacks
    [:div.goal
     (display-card
      (first goal)
      true
      (when yours :goal)
      false
      (count goal))]
    [:div.discards
     (for [d [:d1 :d2 :d3 :d4]]
       (display-card
        (first (d player))
        true
        (when yours d)
        (when yours d)
        (count (d player))
        (rest (d player))))]]])

(def *errors*
     {"invalid" "You can't do that. Try again."
      "nopick"  "You did not pick a card to play."
      "notturn" "Please wait for your turn."})

(defn gameview [req]
  (let [g (get (:params req) "g")
        p (get (:params req) "p")
        game (fetch-by-id Game g)
        players (:players game)
        player (get-player-by-id game p)
        hand (:hand player)
        goal (:goal player)
        yourturn? (= player
                     (first players))
        winner (:name (:winner game))
        error (get (:params req) "e")]
    (base
     {:messages (when (= :post (:request-method req))
                  [:p.error "You can't do that."])
      :pagetitle (cond
                  winner (str winner " wins!")
                  yourturn?
                  "Your Turn!"
                  :else
                  "Cat and Mouse")
      :extrahead (cond
                  winner (html
                          [:script {:type "text/javascript"}
                           (format "alert(\"%s wins!\");"
                                   winner)])
                  (not yourturn?)
                  [:meta {:HTTP-EQUIV "REFRESH"
                          :CONTENT "5"}])
      :pagename (cond
                 winner [:span
                         (str winner " wins! ")
                         (link-to "/"
                                  "Start Over")]
                 yourturn? [:span "Hello " (:name player)
                            ". It is your turn."]
                 :else
                 [:span "Hello " (:name player) ". "
                  "It is " (:name (first players))
                  "'s turn."])
      :content [:div.game
                [:form {:method "POST"
                        :action (format
                                 "/move?g=%s&p=%s"
                                 g p)}
                 [:div.yours
                  (playerview player hand goal true)
                  [:div.playpiles
                   (for [d [:D1 :D2 :D3 :D4]]
                     (display-card
                      (first (d game))
                      true false d))]]
                 [:div.others
                  (for [pl (filter #(not= player %)
                                   players)]
                    (playerview pl
                                (:hand pl)
                                (:goal pl)
                                false))]]]})))

(defn move [req]
  (let [g (get (:params req) "g")
        p (get (:params req) "p")
        game (fetch-by-id Game g)
        player (get-player-by-id game p)
        from (keyword (get (:params req) "playme"))
        t (filter #(= "Drop" (val %))
                  (:form-params req))
        to (when (seq t)
             (keyword
              (key (first t))))
        play (make-move game player from to)]
    (if play
      (do (save play)
          (redirect-to (format "/game?g=%s&p=%s" g p)))
      (gameview req))))
