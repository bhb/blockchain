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
               [#:bc {:sender "abc", :recipient "def", :amount 5}]}
         (add-tx {:bc/chain []
                  :bc/transactions []} "abc" "def" 5))))

(deftest test-next-idx
  (is (= 0
         (next-idx
          #:bc {:chain []
                :transactions
                [#:bc {:sender "abc", :recipient "def", :amount 5}]}))))

(deftest test-blockchain
  (is (s/valid? :bc/bc (blockchain))))

(s/def :bc/ops #{`bc/add-block `bc/add-tx})

(deftest test-block-ops
  (checking
   "all ops result in valid blockchain"
   20
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
    (is (= 10
           (-> (blockchain)
               (add-tx "a" "b" 10)
               (balance "b")
               )
             ))
    )
  )

#_(deftest mine
  (testing "mining rewards miner with a bitcoin"
    (let [bc (blockchain)
          miner (node-id)]
      (is (= 
             (-> bc
                 (add-tx "a" "b" 0.5)
                 (add-tx "b" "c" 0.1)
                 (mine miner)
                 )
             )))
    )
  )
