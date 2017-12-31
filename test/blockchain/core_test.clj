(ns blockchain.core-test
  (:require [clojure.test :refer :all]
            [blockchain.core :refer :all :as bc]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.test.check.generators :as gen]
            [orchestra.spec.test :as st]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]))

(defn instrument [f]
  (set! s/*explain-out* expound/printer)
  (st/instrument)
  (f))

(use-fixtures :once instrument)

(deftest test-add-tx
  (is (= #:bc {:chain []
               :transactions
               [#:bc {:sender "0", :recipient "abc", :amount 5}
                #:bc {:sender "abc", :recipient "def", :amount 5}]}
         (add-tx {:bc/chain []
                  :bc/transactions [#:bc {:sender "0", :recipient "abc", :amount 5}]} "abc" "def" 5)))
  (testing "sender cannot send more money than they have"
    (is (= 0
           (-> (blockchain)
               (add-tx "a" "b" 10)
               (balance "a"))))
    (is (= 0
           (-> (blockchain)
               (add-tx "a" "b" 10)
               (balance "b"))))))

(deftest test-next-idx
  (is (= 0
         (next-idx
          #:bc {:chain []
                :transactions
                [#:bc {:sender "abc", :recipient "def", :amount 5}]}))))

(deftest test-blockchain
  (is (s/valid? :bc/bc (blockchain))))

(s/def :bc/ops #{`bc/add-block `bc/add-tx `bc/mine-fast})

(deftest test-block-ops
  (checking
   "all ops result in valid blockchain"
   10
   [ops (s/gen (s/coll-of :bc/ops))
    op-args (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops))
    :let [op+args (map vector ops op-args)]]
   (let [result (reduce
                 (fn [bc [op args]]
                   (apply @(resolve op)
                          bc
                          (rest args)))
                 (blockchain)
                 op+args)]
     (is (s/valid? :bc/bc result)))))

(deftest test-balance
  (testing "transactions change balance"
    (let [miner (node-id)]
      (is (= 2
             (-> (blockchain)
                 (mine-fast miner)
                 (mine-fast miner)
                 (add-tx miner "b" 2)
                 (balance "b")))))))

(deftest test-mine
  (testing "mining clears out txs"
    (let [bc (blockchain)
          miner (node-id)]
      (is (= []
             (-> bc
                 (add-tx "a" "b" 0.5)
                 (add-tx "b" "c" 0.1)
                 (mine-fast miner)
                 (:bc/transactions))))))
  (testing "mining rewards miner with a bitcoin"
    (let [miner (node-id)]
      (is (= 1
             (-> (blockchain)
                 (mine-fast miner)
                 (balance miner))))))
  (checking
   "money balances in blockchain for single node"
   10
   [ops (s/gen (s/coll-of :bc/ops))
    first-miner (s/gen :bc/recipient)
    op-args (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops))
    :let [op+args (map vector ops op-args)]]
   (let [result (reduce
                 (fn [bc [op args]]
                   (apply @(resolve op)
                          bc
                          (rest args)))
                 (-> (blockchain)
                     (mine-fast first-miner))
                 op+args)]
     (is (zero? (-> result
                    balance-sheet
                    total-balance))
         [:failed {:balance-sheet balance-sheet}]))))
