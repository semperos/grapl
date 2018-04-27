(ns grapl.scratch
  (:require [ubergraph.core :as uber]
            [loom.graph :as graph]))

(def url-graphs-map
  {:nodes [:root
           :state
           :city
           [:? {:cond 'hood
                :then :a
                :else :b}]
           :hood
           :property-type]
   :edges [[:root :state {:sep "/"}]
           [:state :city {:sep "/"}]
           [:city :?]
           [:? :hood {:branch :a
                      :sep "/"}]
           [:? :property-type {:branch :b
                               :sep "-"}]
           [:hood :property-type {:sep "-"}]]})

(def url-graph-cypher
  "Cypher-inspired syntax."
  '[(:root)->["/"]->
    (:state)-["/"]->
    (:city)->
    (? :hood :a :b)
    -[:a "/"]->(:hood)-["-"]->(:property-type)
    -[:b "-"]->(:property-type)])

(def url-graph-str
  "


  |root -\"/\"->
    |state -\"/\"->
      |city -> |? hood
        -\"/\"-> |hood -\"-\"-> |property-type||
        -\"-\"-> |property-type|
      |
    |
  |


  ")

(defprotocol AsGraph
  (as-graph [this] "Convert `this` to a graph"))

(defn graph-from-map
  [{:keys [nodes edges] :as m}]
  (let [g (uber/digraph)
        default-node-attrs (fn [x]
                             (if (coll? x)
                               [(first x) (or (second x) {})]
                               [x {}]))
        default-edge-attrs (fn [[s t a :as edge]]
                             (if-not a [s t {}] edge))
        g (apply uber/add-nodes-with-attrs g (map default-node-attrs nodes))
        g (apply uber/add-directed-edges g (map default-edge-attrs edges))]
    g))

(extend-protocol AsGraph
  clojure.lang.IPersistentMap
  (as-graph [{:keys [nodes edges] :as m}]
    (graph-from-map m)))

(defn eval-node
  [n g]
  )

(defn eval-graph
  "Given a graph and a set of evaluation restrictions, evaluate the graph. In terms of a Lisp's REPL sequence, this is _after_ read."
  [g]
  (when-not (graph/directed? g)
    (throw (IllegalArgumentException. "The graph must be a directed graph. It may contain cycles.")))
  #_(loop [n (first (uber/nodes g)) ret nil]
      (if n
        (if-let [succ (uber/successors )])
        (recur
         (next ns) (eval-node n g))
        ret)))
