;; From https://hackernoon.com/learn-blockchains-by-building-one-117428612f46
(ns blockchain.core
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as string]
            [digest :as digest]))

(defn sha [x]
  (digest/sha-256 (pr-str x)))

;;;;;;;;;;;;;;;;; specs ;;;;;;;;;;;;;;;;;;;

(s/def :bc/sha
  (s/with-gen
    (s/and string?
           #(re-matches #"[A-Fa-f0-9]+" %))
    #(s/gen (into #{} (map (fn [x] (subs (sha x) 0 5)) (range 10))))))

(s/def :bc/sender :bc/sha)
(s/def :bc/recipient :bc/sha)
;; nat-int isn't accurate for a real blockchain, but I don't
;; really care about dealing with floating point math, etc for this exercise
(s/def :bc/amount nat-int?)
(s/def :bc/proof nat-int?)
(s/def :bc/prev-hash :bc/sha)

(s/def :bc/transaction (s/keys :req
                               [:bc/sender
                                :bc/recipient
                                :bc/amount]))
(s/def :bc/transactions (s/coll-of :bc/transaction
                                   :kind vector?))

(s/def :bc/index nat-int?)
(s/def :bc/timestamp inst?)
(s/def :bc/prev-hash :bc/sha)

(s/def :bc/block (s/keys :req
                         [:bc/index
                          :bc/timestamp
                          :bc/transactions
                          :bc/proof
                          :bc/prev-hash]))

(s/def :bc/chain (s/coll-of :bc/block :kind vector?))

(s/def :bc/node :bc/sha)
(s/def :bc/nodes (s/coll-of :bc/node :kind set?))

(s/def :bc/bc (s/keys :req [:bc/transactions
                            :bc/nodes
                            :bc/chain]))

(s/def :bc/registry (s/map-of :bc/node :bc/bc))

;;;;;;;;;;;;;;;;; end specs ;;;;;;;;;;;;;;;;;;;

(def system-node "0")

(def genesis {:bc/prev-hash "1"
              :bc/proof 100})

(defn now [] (new java.util.Date))

(s/fdef add-block
        :args (s/cat :bc :bc/bc
                     :proof :bc/proof
                     :prev-hash (s/? :bc/prev-hash))
        :ret :bc/bc)
(defn add-block
  ([bc proof]
   (add-block bc proof (sha (last (:bc/chain bc)))))
  ([bc proof prev-hash]
   (update bc
           :bc/chain
           conj
           {:bc/index (+ (count (:bc/chain bc)) 1)
            :bc/timestamp (now)
            :bc/transactions (:bc/transactions bc)
            :bc/proof proof
            :bc/prev-hash prev-hash})))

(s/fdef blockchain
        :ret :bc/bc)
(defn blockchain []
  (add-block
   {:bc/transactions []
    :bc/chain []
    :bc/nodes #{}}
   (:bc/proof genesis)
   (:bc/prev-hash genesis)))

(s/fdef next-idx
        :args (s/cat :bc :bc/bc)
        :ret :bc/index)
(defn next-idx [bc]
  (-> bc
      :bc/chain
      last
      :bc/indix
      (or 0)))

(def ^:dynamic *suffix* "0000")

(defn valid-proof? [last-proof guess]
  (string/ends-with? (sha (str last-proof guess)) *suffix*))

(s/fdef proof-of-work
        :args (s/cat :last-proof :bc/proof)
        :ret :bc/proof)
(defn proof-of-work
  "Simple Proof of Work Algorithm:
   - Find a number p' such that hash(pp') contains leading 4 zeroes, where p is the previous p'
   - p is the previous proof, and p' is the new proof"
  [last-proof]
  (->> (range)
       (filter (partial valid-proof? last-proof))
       first))

(defn random-uuid [] (str (java.util.UUID/randomUUID)))

(defn node-id []
  (-> (random-uuid)
      (str)
      (string/replace "-" "")))

(defn transactions [bc]
  (concat (:bc/transactions bc)
          (mapcat
           :bc/transactions
           (:bc/chain bc))))

(defn transactions-for [bc node-id]
  (filter #(or (= node-id (:bc/recipient %))
               (= node-id (:bc/sender %)))
          (transactions bc)))

(defn balance [bc node-id]
  (reduce
   +
   0
   (map (fn [tx]
          (if (= node-id (:bc/recipient tx))
            (:bc/amount tx)
            (* -1 (:bc/amount tx))))
        (transactions-for bc node-id))))

(defn balance-sheet [bc]
  (reduce
   (fn [m tx]
     (s/assert map? m)
     (s/assert :bc/transaction tx)
     (-> m
         (update (:bc/recipient tx) (fnil + 0) (:bc/amount tx))
         (update (:bc/sender tx) (fnil - 0) (:bc/amount tx))))
   {}
   (transactions bc)))

(defn total-balance [balance-sheet]
  (reduce
   +
   0
   (vals balance-sheet)))

(s/fdef add-tx
        :args (s/cat :bc :bc/bc
                     :sender :bc/sender
                     :recipient :bc/recipient
                     :amount :bc/amount)
        :ret :bc/bc)
(defn add-tx [bc sender recipient amount]
  (if (or
       (= sender system-node)
       (<= amount (-> bc balance-sheet (get sender 0))))
    (update bc
            :bc/transactions
            conj
            {:bc/sender sender
             :bc/recipient recipient
             :bc/amount amount})
    bc))

(s/fdef mine
        :args (s/cat :bc :bc/bc
                     :node-id :bc/node)
        :ret :bc/bc)
(defn mine [bc node-id]
  (let [new-proof (proof-of-work (:bc/proof (last (:bc/chain bc))))]
    (-> bc
        (add-tx system-node node-id 1)
        (add-block new-proof)
        (assoc :bc/transactions []))))

;; Just for generative-testing
(s/fdef mine-fast
        :args (s/cat :bc :bc/bc
                     :node-id :bc/recipient)
        :ret :bc/bc)
(defn mine-fast [bc node-id]
  (binding [*suffix* "0"]
    (mine bc node-id)))

(s/fdef add-node
        :args (s/cat :bc :bc/bc
                     :node-id :bc/node))
(defn add-node [bc node]
  (update bc :bc/nodes conj node))

(defn add-nodes [bc nodes]
  (reduce
   add-node
   bc
   nodes))

(s/fdef valid?
        :args (s/cat :bc :bc/bc)
        :ret boolean?)
(defn valid? [bc]
  (every? true?
          (for [[last-block block] (partition 2 1 (:bc/chain bc))]
            (and (valid-proof? (:bc/proof last-block)
                               (:bc/proof block))
                 (= (sha last-block)
                    (:bc/prev-hash block))))))

(s/fdef resolve-conflicts
        :args (s/cat
               :bc :bc/bc
               :other-bcs (s/coll-of :bc/bc))
        :ret :bc/bc)
(defn resolve-conflicts [bc other-bcs]
  (let [candidate (->> (conj other-bcs bc)
                       distinct
                       (filter valid?)
                       (sort-by (comp count :bc/chain))
                       last)]
    (if (< (count (:bc/chain bc))
           (count (:bc/chain candidate)))
      candidate
      bc)))

(s/fdef resolve-known-conflicts
        :args (s/cat :bc :bc/bc
                     :reg :bc/registry)
        :ret :bc/bc)
(defn resolve-known-conflicts
  "Resolve conflicts between known nodes"
  [bc registry]
  (resolve-conflicts
   bc
   (or (->> bc
            :bc/nodes
            (select-keys registry)
            vals)
       [])))
(comment
  (require '[orchestra.spec.test :as st])
  (s/check-asserts true)
  (set! s/*explain-out* expound/printer)
  (st/instrument))
