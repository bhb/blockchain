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
(def num-tests 20)

(deftest test-add-tx
  (is (= #:bc {:chain []
               :nodes #{}
               :transactions
               [#:bc {:sender "0", :recipient "abc", :amount 5}
                #:bc {:sender "abc", :recipient "def", :amount 5}]}
         (add-tx {:bc/chain []
                  :bc/nodes #{}
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
                :nodes #{}
                :transactions
                [#:bc {:sender "abc", :recipient "def", :amount 5}]}))))

(deftest test-blockchain
  (is (s/valid? :bc/bc (blockchain))
      (expound/expound-str :bc/bc (blockchain))))

(s/def :bc/ops #{`bc/add-tx `bc/mine-fast `bc/add-node})

(deftest test-block-ops
  (checking
   "all ops result in valid blockchain"
   num-tests
   [ops (s/gen (s/coll-of :bc/ops))
    op-args (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops))
    :let [op+args (map vector ops op-args)
          result (reduce
                  (fn [bc [op args]]
                    (apply @(resolve op)
                           bc
                           (rest args)))
                  (blockchain)
                  op+args)]]
   (is (s/valid? :bc/bc result)
       (expound/expound-str :bc/bc result))
   (is (binding [bc/*suffix* "0"]
         (valid? result)))))

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
   num-tests
   [ops (s/gen (s/coll-of :bc/ops))
    first-miner (s/gen :bc/recipient)
    op-args (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops))
    :let [op+args (map vector ops op-args)
          result (reduce
                  (fn [bc [op args]]
                    (apply @(resolve op)
                           bc
                           (rest args)))
                  (-> (blockchain)
                      (mine-fast first-miner))
                  op+args)]]
   (is (zero? (-> result
                  balance-sheet
                  total-balance))
       [:failed {:balance-sheet balance-sheet}])))

(deftest test-resolve-conflicts
  (testing "prefers current blockchain if both are tied"
    (let [bc1 (-> (blockchain)
                  (mine-fast "abc"))
          bc2 (-> (blockchain)
                  (mine-fast "def"))]
      (binding [bc/*suffix* "0"]
        (is (= bc1 (resolve-conflicts bc1 [bc2]))))))
  (testing "longer blockchains win"
    (let [bc1 (-> (blockchain)
                  (mine-fast "a"))
          bc2 (-> (blockchain)
                  (mine-fast "b")
                  (mine-fast "c"))]
      (binding [bc/*suffix* "0"]
        (is (= bc2 (resolve-conflicts bc1 [bc2])))
        (is (= bc2 (resolve-conflicts bc2 [bc1])))
        (is (= bc1 (resolve-conflicts bc1 [bc1]))))))
  (checking
   "for any two valid blockchains of unequal length, conflict resolution is consistent"
   num-tests
   [ops1 (s/gen (s/coll-of :bc/ops))
    op-args1 (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops1))
    ops2 (s/gen (s/coll-of :bc/ops))
    op-args2 (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops2))
    :let [op+args1 (map vector ops1 op-args1)
          op+args2 (map vector ops2 op-args2)
          bc1 (reduce
               (fn [bc [op args]]
                 (apply @(resolve op)
                        bc
                        (rest args)))
               (blockchain)
               op+args1)
          bc2 (reduce
               (fn [bc [op args]]
                 (apply @(resolve op)
                        bc
                        (rest args)))
               (blockchain)
               op+args2)]]
   (binding [bc/*suffix* "0"]
     (is (valid? bc1))
     (is (valid? bc2))
     (when (not= (count (:bc/chain bc1))
                 (count (:bc/chain bc2)))
       (is (= (resolve-conflicts bc1 [bc2])
              (resolve-conflicts bc2 [bc1])))))))

(defn map-vals [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(deftest test-resolution
  ;; Set the suffix, required for all validation and conflict resolution
  (binding [bc/*suffix* "0"]
    (checking
     "adding nodes always resolves blockchain conflicts"
     num-tests
     [ops (s/gen (s/coll-of #{`bc/add-tx `bc/mine-fast `bc/add-node}))
      :when (pos? (count ops))
      nodes (gen/vector (s/gen :bc/node) (count ops))
      op-args (apply gen/tuple (map #(s/gen (:args (s/spec %))) ops))
      :let [registry (reduce
                      (fn [registry [node op args]]
                        (let [bc (get registry node (blockchain))]
                          (assoc registry node
                                 (apply @(resolve op)
                                        bc
                                        (rest args)))))
                      {}
                      (map vector nodes ops op-args))
            resolved-registry (let [r' (map-vals registry add-nodes nodes)]
                                (map-vals r' #(resolve-known-conflicts % r')))
            bcs (vals resolved-registry)]]
     ;; Unless there is a tie for longest blockchain ...
     (when-not (< 1 (count (val (last (sort (group-by (comp count :bc/chain) (vals registry)))))))
       ;; then there should be consensus
       (is (apply = bcs)
           (str "Sequence is:\n"
                (apply str (map
                            (fn [node op args]
                              (str "node " node " :: " `(~op ~@(rest args)) "\n"))
                            nodes
                            ops
                            op-args))
                "\nChain lengths:\n"
                (apply str (for [[node bc] resolved-registry]
                             (str "node " node ": " (count (:bc/chain bc)) "\n")))))))))
