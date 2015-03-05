(ns monopoly.main
  (:require [clojure.pprint :refer [pprint print-table]]
            [clojure.core.async :as async :refer [<! >! <!!]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

(def places
  [:go
   :old-kent-road
   :community-chest
   :whitechapel-road
   :income-tax
   :kings-cross-station
   :the-angel-islington
   :chance
   :euston-road
   :pentonville-road
   :jail
   :pall-mall
   :electric-company
   :whitehall
   :northumberland-avenue
   :waterloo-station
   :bow-street
   :community-chest
   :marlborough-street
   :vine-street
   :free-parking
   :strand
   :chance
   :fleet-street
   :trafalgar-square
   :fenchurch-st-station
   :leicester-square
   :coventry-street
   :water-works
   :picadilly
   :go-to-jail
   :oxford-street
   :regent-street
   :community-chest
   :bond-street
   :liverpool-st-station
   :chance
   :park-lane
   :super-tax
   :mayfair])

(def place->index (into {} (for [[i place] (map-indexed vector places)]
                             [place i])))

(defn roll-chance []
  "Get random chance card from deck
  Only chance cards relevant to common position analysis are codified here"
  (case (rand-int 16)
    0 {:type :advance, :to :mayfair}
    1 {:type :advance, :to :go}
    2 {:type :go-to-jail}
    3 {:type :move-delta :delta -3}
    4 {:type :get-out-of-jail-free}
    5 {:type :advance, :to :trafalgar-square}
    nil))

(defn roll-community-chest []
  "Get random community chest card from deck
  Only community chest cards relevant to common position analysis are codified here"
  (case (rand-int 16)
    0 {:type :get-out-of-jail-free}
    1 {:type :advance, :to :go}
    2 {:type :go-to-jail}
    3 {:type :take-chance-card}
    4 {:type :advance, :to :old-kent-road}
    nil))

(defn single-die-roll []
  "Random number 1 -> 6 inclusive"
  (+ (rand-int 6) 1))

(defn non-recursive-roll []
  "Rolls two dice and returns [total score, were they doubles?]"
  (let [[roll-1 roll-2] [(single-die-roll) (single-die-roll)]]
    [(+ roll-1 roll-2) (= roll-1 roll-2)]))

(defn recursive-dice-roll [num-rolls]
  "Rolls two dice, re-rolling for doubles

  Returns the total score unless num-rolls is exceeded (e.g. three doubles), in which
  case, returns :jail"
  (loop [iterations-left num-rolls
         accum 0]
    (if (zero? iterations-left) :jail
      (let [[roll doubles?] (non-recursive-roll)
            total (+ accum roll)]
        (if (not doubles?) total
          (recur (dec iterations-left)
                 total))))))

(def default-rolls 3)

(def init-state
  {:position 0
   :rolls default-rolls
   :get-out-of-jail-free-cards 0
   :turns-left-in-jail 0})

(def in-jail? (comp pos? :turns-left-in-jail))
(def any-get-out-of-jail-free-cards? (comp pos? :get-out-of-jail-free-cards))
(def leave-jail #(assoc % :turns-left-in-jail 0))
(def use-get-out-of-jail-free-card #(-> % (update-in [:get-out-of-jail-free-cards] dec)
                                          leave-jail))
(def reset-rolls #(assoc % :rolls default-rolls))
(def get-place-name (comp places :position))

(defn advance-position [state spaces]
  "Advance position along board. If the total number of places is passed, wraps back round"
  (update-in state [:position] #(mod (+ % spaces) (count places))))

(defn go-to-jail [state]
  "Move 'player' to jail"
  (merge state {:position (place->index :jail)
                :turns-left-in-jail 3}))

(defmacro if->
  "Threads an expression through to one of two forms depending on the truth-value of `test`
  Just returns `expr` if `else-form` is blank and `test` is false

  e.g. (if-> ' weather, isnt it?' is-rainy-day? (str 'Terrible') (str 'Lovely'))"
  ([expr test then-form else-form]
   `(let [expr# ~expr]
      (if ~test (-> expr# ~then-form)
        (-> expr# ~else-form))))
  ([expr test then-form]
   `(if-> ~expr ~test ~then-form identity)))

(defn attempt-doubles-jail-escape [state]
  "Attempt to escape jail with double dice roll"
  (let [[roll doubles?] (non-recursive-roll)]
    (if-> state doubles?
          (-> leave-jail
              (advance-position roll)
              (update-in [:rolls] dec)))))

(defn get-out-of-jail [state]
  "Attempt to get out of jail by waiting a turn, using a get-out-of-jail-free card or
  rolling doubles"
  (as-> state state'
    (update-in state' [:turns-left-in-jail] dec)
    (if-> state' (and (in-jail? state') (any-get-out-of-jail-free-cards? state'))
          use-get-out-of-jail-free-card)
    (if-> state' (in-jail? state')
          attempt-doubles-jail-escape)))

(defn roll-dice [state]
  "Roll dice and either move or go directly to jail depending on number of doubles thrown"
  (let [roll (recursive-dice-roll (:rolls state))]
    (if-> state (= roll :jail)
          go-to-jail
          (advance-position roll))))

(defn process-card [state card]
  "Process chance or community chest card"
  (as-> state state'
    (case (:type card)
      :advance (assoc state' :position (place->index (:to card)))
      :go-to-jail (go-to-jail state')
      :move-delta (advance-position state' (:delta card))
      :get-out-of-jail-free (update-in state' [:get-out-of-jail-free-cards] inc)
      :take-chance-card (process-card state' (roll-chance))
      state')))

(defn tick [state]
  "One tick through for a single player. Implements movement, jail and chance / community chest logic"
  (as-> state state'
    (reset-rolls state')
    (if-> state' (in-jail? state') get-out-of-jail)
    (if-> state' (not (in-jail? state')) roll-dice)
    (case (get-place-name state')
      :chance (process-card state' (roll-chance))
      :community-chest (process-card state' (roll-community-chest))
      state')))

(defn tick-forever
  "Lazy sequence that will tick a player through the game indefinitely, returning each player
  state along the way"
  ([] (tick-forever init-state))
  ([state] (cons state (lazy-seq (tick-forever (tick state))))))

(defn run-worker [turns]
  "Run a single worker through the game
  Returns a channel that each position is fed into until the number of turns is over"
  (let [chan (async/chan 10 (map :position))]
    (async/go
      (doseq [state (take turns (tick-forever))]
        (>! chan state))
      (async/close! chan))
    chan))

(defn run-all [num-turns num-workers]
  "Creates a batch of workers to run through the game concurrently
  Returns a map mapping position number -> number of hits"
  (let [workers (take num-workers (repeatedly (partial run-worker num-turns)))
        workers-chan (async/merge workers 10)
        results-chan (async/chan)]
    (async/go-loop [positions->hits (zipmap (range (count places)) (repeat 0))]
                   (let [pos (<! workers-chan)]
                     (if (nil? pos)
                       (do (async/close! workers-chan)
                           (>! results-chan positions->hits))
                       (recur (update-in positions->hits [pos] inc)))))
    (<!! results-chan)))

(defn process-results [positions->hits]
  "Transform positions->hits map into a list of results for each position
  Each result includes position, property name, number of hits and a percentage of hits"
  (let [as-maps (for [[position hits] positions->hits]
                  {:position position, :name (places position), :hits hits})
        total-hits (reduce + (map :hits as-maps))]
    (map (fn [{:keys [hits] :as result}]
           (assoc result :percent (-> hits (/ total-hits) (* 100))))
         as-maps)))

(defn print-results [results]
  (let [sorted (reverse (sort-by :hits results))]
    (print-table
      (for [{:keys [position name hits percent]} (reverse (sort-by :hits results))]
        {"Position" position, "Name" name,
         "Hits" hits, "Percent" (format "%.1f%%" (double percent))}))))

(defn positive-number-cli-option [config]
  (concat config
    [:parse-fn #(Integer/parseInt %)
     :validate [(partial < 1) "Must be a positive number"]]))

(def cli-options
  [(positive-number-cli-option
     [nil "--turns TURNS" "Number of turns"
      :default 30])
   (positive-number-cli-option
     [nil "--workers WORKERS" "Number of workers"
      :default 20])
   ["-h" "--help" "Show this help"]])

(defn -main
  [& args]
  (let [{{:keys [turns workers help]} :options, summary :summary} (parse-opts args cli-options)]
    (if help (println summary)
      (let [results (process-results (run-all turns workers))]
          (print-results results)))))
