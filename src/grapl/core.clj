(ns grapl.core
  (:require [clojure.pprint :refer [pp pprint]]
            [clojure.string :as str]
            [loom.graph :as graph]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [gulfstream.core :as gs]
            [gulfstream.graph :as gg]))

(def class-names
  {clojure.lang.PersistentHashMap "hash-map"
   clojure.lang.PersistentHashSet "hash-set"
   clojure.lang.PersistentList "list"
   clojure.lang.PersistentQueue "queue"
   clojure.lang.PersistentTreeMap "tree-map"
   clojure.lang.PersistentTreeSet "tree-set"
   clojure.lang.PersistentVector "vector"})

(def class-labels
  {clojure.lang.PersistentHashMap "{...}"
   clojure.lang.PersistentHashSet "#{...}"
   clojure.lang.PersistentList "(...)"
   clojure.lang.PersistentQueue ">-[...]->"
   clojure.lang.PersistentTreeMap "{...}"
   clojure.lang.PersistentTreeSet "#{...}"
   clojure.lang.PersistentVector "[...]"})

(defn uniq [x]
  (if (coll? x)
    (let [raw (str (get class-names (class x) (.getName (class x)))
                   (gensym "_"))]
      (if-let [idx (str/last-index-of raw ".")]
        (keyword (subs raw (inc idx)))
        (keyword raw)))
    (str (pr-str x) (gensym "_"))))

(defn form-graph*
  ([form] (form-graph* (uber/digraph) form))
  ([g form]
   (cond
     (list? form) ;; TODO Make this configurable
     (let [forms form
           n (uniq forms)
           label (get class-labels (class forms) "coll")
           g (uber/add-nodes-with-attrs g [n {:label label}])
           {:keys [g ns]} (reduce
                           (fn [{:keys [g ns]} form]
                             (let [{:keys [node graph]} (form-graph* g form)]
                               {:g (uber/add-edges graph [n node])
                                :ns (conj ns node)}))
                           {:g g
                            :ns []}
                           forms)
           g (loop [g g ns ns]
               (if ns
                 (if-let [next-n (first (next ns))]
                   (recur (uber/add-edges g [(first ns) next-n {:ui.class "list"}])
                          (next ns))
                   g)
                 g))]
       {:node n
        :graph g})

     (coll? form)
     (let [forms form
           n (uniq forms)
           label (get class-labels (class forms) "coll")
           g (uber/add-nodes-with-attrs g [n {:label label}])]
       {:node n
        :graph (reduce
                (fn [g form]
                  (let [{:keys [node graph]} (form-graph* g form)]
                    (uber/add-edges graph [n node])))
                g
                forms)})

     :else
     (let [n (uniq form)]
       {:node n
        :graph (uber/add-nodes-with-attrs g [n {:label form}])}))))

(defn form-graph
  [form]
  (:graph (form-graph* (uber/digraph) form)))

(defn gs-nodes
  [ug]
  (into {}
        (comp (map (juxt identity (partial uber/attrs ug)))
              (map (fn [[id attrs]] (if (:label attrs)
                                      [id attrs]
                                      [id (assoc attrs :label id)]))))
        (uber/nodes ug)))

(defn gs-edges
  [ug]
  (let [raw-edges (uber/edges ug)
        attrs (map (partial uber/attrs ug) raw-edges)
        edge-pairs (map (juxt :src :dest) raw-edges)]
    (->> (interleave edge-pairs attrs)
         (apply hash-map))))

(defn gs-dom
  "Create gulfstream dom structure from an ubergraph"
  [ug]
  {:nodes (gs-nodes ug)
   :edges (gs-edges ug)})

(defn gs-set-pos
  [g n x y]
  (.setAttribute (.getNode g (name n)) "xyz" (into-array Object [x y 0]))
  g)

(defn gs-get-pos
  [g n]
  (.getAttribute (.getNode g (name n)) "xyz"))

(defn manual-layout
  "Browser is a GraphStream browser, ug is an ubergraph graph."
  [browser ug]
  (let [[viewer graph] ((juxt :viewer :graph) browser)
        edges (uber/edges ug)]
    (.disableAutoLayout viewer)
    (let [nodes (alg/topsort ug)]
      (gs-set-pos graph (first nodes) 1 1)
      (doseq [[nidx n] (map-indexed (fn [idx x] [idx x]) nodes)
              :let [children (map-indexed (fn [idx x] [idx x])
                                          (uber/successors ug n))
                    [nx ny _] (gs-get-pos graph n)]]
        (when (seq children)
          (doseq [[idx child] children]
            (let [x (+ idx #_(* 1.2 idx) nx)
                  y (- ny 1 #_(* 0.2 nidx))]
              (gs-set-pos graph child x y))))))))

(def sample-ubergraph
  (form-graph '(do
                 (defn identity [x] x)
                 (defn add [x y] (+ x y)))))

(def sample-browser
  (doto (gs/browse {:title "Code Graph"
                    :attributes {:gs.disable-auto-layout true}
                    :dom (gs-dom sample-ubergraph)
                    :style [[:node {:fill-color "white"
                                    :text-size 12
                                    :text-color "black"
                                    :text-alignment "at-left"
                                    :stroke-mode "plain"
                                    :stroke-width 2
                                    :stroke-color "orange"}]
                            [(keyword "node:clicked") {:stroke-color "purple"}]
                            [:edge.list {:stroke-mode "dashes"
                                         :size 0.2
                                         :fill-color "#efefef"}]]})
    (manual-layout sample-ubergraph)))
