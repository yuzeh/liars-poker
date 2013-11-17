(ns liars-poker.core
  (:require [clojure.contrib.def :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.tools.cli :as cli]
            [bigml.sampling.simple :as simple])
  (:gen-class))

(defstruct Card :suit :rank)

(def suits [:♠ :♥ :♣ :♦])

(def ranks (range 13)) ;; 0 -> 3, 1 -> 4, ..., 11 -> A, 12 -> 2

(def deck (into [] (map #(apply struct Card %) (combo/cartesian-product suits ranks))))

(defn wild? [card] (= 12 (:rank card)))

(defn flatten-rank [hand]
  (let [card-ranks (map :rank hand)
        rank-map (reduce (fn [m x] (assoc m x (inc (m x 0)))) {} card-ranks)]
    (into [] (map #(rank-map % 0) ranks))))

(defn flatten-rank-by-suit
  "Returns a map of suit to rank vector (values à la flatten-rank)"
  [hand]
  (into {}
    (for [suit suits
          :let [cards-of-suit (filter #(= suit (:suit %)) hand)]]
      [suit (flatten-rank cards-of-suit)])))

(defnk flatten-suit [hand :wilds false]
  (let [hand (if wilds hand (filter (complement wild?) hand))
        card-suits (map :suit hand)]
    (reduce (fn [m x] (assoc m x (inc (m x 0)))) {:♠ 0 :♥ 0 :♣ 0 :♦ 0} card-suits)))

(defn count-wilds [hand] (count (filter wild? hand)))

(defn index-map [f indices coll] (map #(f (nth coll %)) indices))

(defn generalized-straight-at?
  "Returns truthy if there is a straight starting at start-rank.
   start-rank ranges from 0 (3) to 7 (10), with a special case 11 (A) which checks the existence of
   the A-2-3-4-5 straight.

   The width parameter describes how many parallel straights there need be. e.g.
     - 1 is normal straight
     - 2 is a 'date.'

   The length parameter describes how long the straight has to be. e.g. 5 is a normal straight."
  [width length rank-counts num-wilds start-rank]
  (let [rank-counts (assoc rank-counts 12 0) ;; turn off wild
        rank-indices (if (= start-rank 11)
                       (concat [11 12] (range (- length 2)))
                       (range start-rank (+ length start-rank)))
        rank-deficits (index-map #(max 0 (- width %)) rank-indices rank-counts)]
    (>= num-wilds (reduce + 0 rank-deficits))))

(def straight-at?      (partial generalized-straight-at? 1 5))
(def date-at?          (partial generalized-straight-at? 2 5))
(def long-straight-at? (partial generalized-straight-at? 1 10))

(def straight-rank-starters (cons 11 (range 8)))
(def long-straight-rank-starters (cons 11 (range 3)))

(defn straight? [hand]
  (let [rank-flattened-hand (flatten-rank hand)
        num-wilds (nth rank-flattened-hand 12)]
    (some (partial straight-at? rank-flattened-hand num-wilds) straight-rank-starters)))

(defn date? [hand]
  (let [rank-flattened-hand (flatten-rank hand)
        num-wilds (nth rank-flattened-hand 12)]
    (some (partial date-at? rank-flattened-hand num-wilds) straight-rank-starters)))

(defn long-straight? [hand]
  (let [rank-flattened-hand (flatten-rank hand)
        num-wilds (nth rank-flattened-hand 12)]
    (some (partial long-straight-at? rank-flattened-hand num-wilds) long-straight-rank-starters)))

(defn flush?
  ([n hand]
    (let [suit-flattened-hand (flatten-suit hand)
          num-wilds (count-wilds hand)]
      (some #(<= n (+ num-wilds %)) (vals suit-flattened-hand))))
  ([hand] (flush? 5 hand)))

(def super-flush? (partial flush? 10))

(defn straight-flush? [hand]
  (some identity
    (for [[suit rank-vector] (flatten-rank-by-suit hand)]
      (some (partial straight-at? rank-vector (nth rank-vector 12)) straight-rank-starters))))

;; full house and friends
(defn full-house?
  ([f1 f2 hand]
    (let [rank-flattened-hand (flatten-rank hand)
          num-wilds (nth rank-flattened-hand 12)
          rank-flattened-hand-without-wild (drop-last rank-flattened-hand)
          sorted-rank-flattened-hand-without-wild (reverse (sort rank-flattened-hand-without-wild))
          r1 (first sorted-rank-flattened-hand-without-wild)
          r2 (second sorted-rank-flattened-hand-without-wild)
          m1 (max 0 (- f1 r1))
          m2 (max 0 (- f2 r2))
          num-missing (+ m1 m2)]
      (<= num-missing num-wilds)))
  ([hand] (full-house? 3 2 hand)))

(def ronald-mcdonald-house? (partial full-house? 4 3))

;; n-of-a-kind
(defn n-of-a-kind? [n hand]
  (let [rank-flattened-hand (flatten-rank hand)
        num-wilds (nth rank-flattened-hand 12)
        most-popular-rank-count (-> rank-flattened-hand
                                    drop-last
                                    sort
                                    reverse
                                    first)]
    (<= n (+ num-wilds most-popular-rank-count))))

(def three-of-a-kind? (partial n-of-a-kind? 3))
(def four-of-a-kind?  (partial n-of-a-kind? 4))
(def five-of-a-kind?  (partial n-of-a-kind? 5))
(def six-of-a-kind?   (partial n-of-a-kind? 6))
(def seven-of-a-kind? (partial n-of-a-kind? 7))
(def eight-of-a-kind? (partial n-of-a-kind? 8))

(def hand->check-function {:three-of-a-kind three-of-a-kind?
                           :four-of-a-kind four-of-a-kind?
                           :five-of-a-kind five-of-a-kind?
                           :six-of-a-kind six-of-a-kind?
                           :seven-of-a-kind seven-of-a-kind?
                           :eight-of-a-kind eight-of-a-kind?
                           :straight straight?
                           :date date?
                           :long-straight long-straight?
                           :flush flush?
                           :super-flush super-flush?
                           :straight-flush straight-flush?
                           :full-house full-house?
                           :ronald-mcdonald-house ronald-mcdonald-house?})

(defn check-sample [hand count-map]
  (let [hand-types (keys hand->check-function)
        present-hand-types (filter #((hand->check-function %) hand) hand-types)]
    (reduce (fn [m k] (assoc m k (inc (m k)))) count-map present-hand-types)))

(defn counts-to-probs [count-map total]
  (into {} (for [[k v] count-map] [k (/ v (float total))])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [[options args banner]
          (cli/cli args
                   ["-n" "--num-cards" "Number of cards" :parse-fn #(Integer. %) :default 20]
                   ["-s" "--num-samples" "Number of samples" :parse-fn #(Integer. %) :default 1000]
                   ["-p" "--print-every" "How often to print results" :parse-fn #(Integer. %) :default 1000])
        num-cards (:num-cards options)
        num-samples (:num-samples options)
        print-every (:print-every options)
        output (loop [i 0
                      count-map (zipmap (keys hand->check-function) (repeat (count hand->check-function) 0))]
                 (let [hand (take num-cards (simple/sample deck))]
                   (when
                     (and (not= 0 i) (= 0 (mod i print-every)))
                     (println (str i ": " (counts-to-probs count-map i))))
                   (if (>= i num-samples)
                       count-map
                       (recur (inc i) (check-sample hand count-map)))))]
    (println (counts-to-probs output num-samples))))

