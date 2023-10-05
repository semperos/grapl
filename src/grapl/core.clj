(ns grapl.core
  (:require
   [clojure.string :as str]
   [ubergraph.core :as uber]))

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

(def counter (java.util.concurrent.atomic.AtomicLong. 0))

(defn uniq [x]
  (if (coll? x)
    (let [raw (str (get class-names (class x) (.getName (class x)))
                   "_"
                   (.incrementAndGet counter))]
      (if-let [idx (str/last-index-of raw ".")]
        (keyword (subs raw (inc idx)))
        (keyword raw)))
    (str (pr-str x) "_" (.incrementAndGet counter))))

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

(comment
  (def sample-ubergraph
    (form-graph '(+ (- 10 2) 3 4 5)))

  (ubergraph.core/viz-graph sample-ubergraph)

  '(do
     (defn identity [x] x)
     (defn add [x y] (+ x y)))

  )
