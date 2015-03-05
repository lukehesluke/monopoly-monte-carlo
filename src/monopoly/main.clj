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
  (case (rand-int 16)
    0 {:type :advance, :to :mayfair}
    1 {:type :advance, :to :go}
    2 {:type :go-to-jail}
    3 {:type :move-delta :delta -3}
    4 {:type :get-out-of-jail-free}
    5 {:type :advance, :to :trafalgar-square}
    nil))

(defn roll-community-chest []
  (case (rand-int 16)
    0 {:type :get-out-of-jail-free}
    1 {:type :advance, :to :go}
    2 {:type :go-to-jail}
    3 {:type :take-chance-card}
    4 {:type :advance, :to :old-kent-road}
    nil))

(defn single-die-roll []
  (+ (rand-int 6) 1))

(defn non-recursive-roll []
  "Returns [total, doubles?]"
  (let [[roll-1 roll-2] [(single-die-roll) (single-die-roll)]]
    [(+ roll-1 roll-2) (= roll-1 roll-2)]))

(defn recursive-dice-roll [num-rolls]
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
(def get-out-of-jail-free-cards? (comp pos? :get-out-of-jail-free-cards))
(def leave-jail #(assoc % :turns-left-in-jail 0))
(def use-get-out-of-jail-free-card #(-> % (update-in [:get-out-of-jail-free-cards] dec)
                                          leave-jail))
(def reset-rolls #(assoc % :rolls default-rolls))
(def get-place-name (comp places :position))

(defn advance-position [state spaces]
  (update-in state [:position] #(mod (+ % spaces) (count places))))

(defn go-to-jail [state]
  (merge state {:position (place->index :jail)
                :turns-left-in-jail 3}))

(defmacro if->
  ([expr test then-form else-form]
   `(let [expr# ~expr]
      (if ~test (-> expr# ~then-form)
        (-> expr# ~else-form))))
  ([expr test then-form]
   `(if-> ~expr ~test ~then-form identity)))

(defn attempt-doubles-jail-escape [state]
  (let [[roll doubles?] (non-recursive-roll)]
    (if-> state doubles?
          (-> leave-jail
              (advance-position roll)
              (update-in [:rolls] dec)))))

(defn get-out-of-jail [state]
  (as-> state state'
    (update-in state' [:turns-left-in-jail] dec)
    (if-> state' (and (in-jail? state') (get-out-of-jail-free-cards? state'))
          use-get-out-of-jail-free-card)
    (if-> state' (in-jail? state')
          attempt-doubles-jail-escape)))

(defn roll-dice [state]
  (let [roll (recursive-dice-roll (:rolls state))]
    (if-> state (= roll :jail)
          go-to-jail
          (advance-position roll))))

(defn process-card [state card]
  "Process chance or community card"
  (as-> state state'
    (case (:type card)
      :advance (assoc state' :position (place->index (:to card)))
      :go-to-jail (go-to-jail state')
      :move-delta (advance-position state' (:delta card))
      :get-out-of-jail-free (update-in state' [:get-out-of-jail-free-cards] inc)
      :take-chance-card (process-card state' (roll-chance))
      state')))

(defn tick [state]
  (as-> state state'
    (reset-rolls state')
    (if-> state' (in-jail? state') get-out-of-jail)
    (if-> state' (not (in-jail? state')) roll-dice)
    (case (get-place-name state')
      :chance (process-card state' (roll-chance))
      :community-chest (process-card state' (roll-community-chest))
      state')))

(defn tick-forever
  ([] (tick-forever init-state))
  ([state] (cons state (lazy-seq (tick-forever (tick state))))))

(defn run-worker [turns]
  (let [chan (async/chan 10 (map :position))]
    (async/go
      (doseq [state (take turns (tick-forever))]
        (>! chan state))
      (async/close! chan))
    chan))

(defn run-all [num-turns num-workers]
  (let [workers (take num-workers (repeatedly (partial run-worker num-turns)))
        workers-chan (async/merge workers 10)
        results-chan (async/chan)]
    (async/go-loop [position-counts (zipmap (range (count places)) (repeat 0))]
                   (let [pos (<! workers-chan)]
                     (if (nil? pos)
                       (do (async/close! workers-chan)
                           (>! results-chan position-counts))
                       (recur (update-in position-counts [pos] inc)))))
    (<!! results-chan)))

(defn process-results [position-counts]
  (let [as-maps (for [[position hits] position-counts]
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
