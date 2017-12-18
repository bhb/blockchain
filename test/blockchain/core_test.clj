(ns blockchain.core-test
  (:require [clojure.test :refer :all]
            [blockchain.core :refer :all]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [orchestra.spec.test :as st]))

(defn instrument [f]
  (set! s/*explain-out* expound/printer)
  (st/instrument)
  (f))

(use-fixtures :once instrument)

(deftest test-add-tx
  (is (= #:bc {:chain [],
               :transactions
               [#:bc {:sender "abc", :recipient "def", :amount 5}]}
         (add-tx {:bc/chain []
                  :bc/transactions []} "abc" "def" 5))))

(deftest test-next-idx
  (is (= 0
         (next-idx
          #:bc {:chain [],
                :transactions
                [#:bc {:sender "abc", :recipient "def", :amount 5}]}))))
