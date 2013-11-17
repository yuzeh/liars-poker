(ns liars-poker.core-test
  (:require [clojure.test :refer :all]
            [liars-poker.core :refer :all]))

(def wilds (into [] (for [suit suits] (struct Card suit 12))))

(deftest four-of-a-kind-test
  (let [four-of-a-kind-hand (into [] (for [suit suits] (struct Card suit 4)))
        missing-one (drop-last four-of-a-kind-hand)
        missing-two (drop-last missing-one)
        missing-three (drop-last missing-two)
        missing-one-one-wild (concat (take 1 wilds) missing-one)
        missing-two-one-wild (concat (take 1 wilds) missing-two)
        missing-two-two-wilds (concat (take 2 wilds) missing-two)
        missing-two-four-wilds (concat wilds missing-two)
        missing-three-three-wilds (concat (take 3 wilds) missing-three)]
    (testing "Four-of-a-kind check works"
      (is (four-of-a-kind? four-of-a-kind-hand)))
    (testing "Four-of-a-kind negative check works"
      (is (not (four-of-a-kind? missing-one)))
      (is (not (four-of-a-kind? missing-two)))
      (is (not (four-of-a-kind? missing-three))))
    (testing "Four-of-a-kind with wild cards work"
      (is (four-of-a-kind? missing-one-one-wild))
      (is (four-of-a-kind? missing-two-two-wilds))
      (is (four-of-a-kind? missing-three-three-wilds))
      (is (four-of-a-kind? wilds))
      (is (not (four-of-a-kind? missing-two-one-wild)))
      (is (four-of-a-kind? missing-two-four-wilds)))))

(deftest five-of-a-kind-test
  (let [four-of-a-kind-hand (into [] (for [suit suits] (struct Card suit 4)))
        four-one-hand (concat (take 1 wilds) four-of-a-kind-hand)
        four-two-hand (concat (take 2 wilds) four-of-a-kind-hand)
        three-one-hand (concat (take 1 wilds) (drop 1 four-of-a-kind-hand))
        three-two-hand (concat (take 2 wilds) (drop 1 four-of-a-kind-hand))
        three-three-hand (concat (take 3 wilds) (drop 1 four-of-a-kind-hand))]
    (testing "Five-of-a-kind check works"
      (is (five-of-a-kind? four-one-hand))
      (is (five-of-a-kind? four-two-hand))
      (is (five-of-a-kind? three-two-hand))
      (is (five-of-a-kind? three-three-hand))
      (is (not (five-of-a-kind? three-one-hand))))))

(def straight (map #(struct Card (nth suits 1) %) (range 5)))
(def special-straight (map #(struct Card (nth suits 1) %) [11 12 0 1 2]))

(deftest straight-test
  (let [not-straight (cons (struct Card :♣ 8) (drop 1 straight))
        straight-with-wild (cons (nth wilds 0) (drop 1 straight))
        straight-with-wild-2 (cons (nth wilds 0) not-straight)
        straight-with-two-wilds (concat (take 2 wilds) (drop 2 straight))]
    (testing "Basic straight checks"
      (is (straight? straight))
      (is (straight? special-straight))
      (is (not (straight? not-straight)))
      (is (straight? straight-with-wild))
      (is (straight? straight-with-wild-2))
      (is (straight? straight-with-two-wilds)))))

(deftest flush-test
  (let [flush-five (for [i (range 5)] (struct Card :♣ i))
        not-flush-four (cons (struct Card :♠ 2) (for [i (range 4)] (struct Card :♣ i)))
        flush-four-one (concat (take 1 wilds) not-flush-four)
        flush-six (for [i (range 6)] (struct Card :♣ i))]
    (testing "Basic flush checks"
      (is (flush? flush-five))
      (is (not (flush? not-flush-four)))
      (is (flush? flush-four-one))
      (is (flush? flush-six)))))
