(ns matcher-starter.graph)

(defn optimal-graph-coloring []

  ;; taking all inputs (number of graphs, number of nodes in the graph, number of edges in the graph) from the user

  (println "Enter Number of Graphs(m) : ") (flush) (def m (read)) ;; (println m)

  (println "Enter Number of Nodes(n) : ") (flush) (def n (read)) ;; (println n)

  (println "Enter Number of Edges(k) : ") (flush) (def k (read)) ;; (println k)

  (println "Enter all Edges of the Graph : ") (flush)
  (newline)
  (def takingedges (apply list (for [x (range (inc k))] (list (read-line)))))
  (def finaledges (rest takingedges))

  (println "Edges of the Graph : " finaledges)

  (def nums (flatten finaledges))


  ;; 'cal-graph-nodes' function to determine all the graphnodes in the given graph problem

  (defn cal-graph-nodes [gn]
    (def findnode (first gn))
    (if-not (empty? findnode)
      (distinct (flatten (cons (partition 2 (map #(Integer/parseInt %) (clojure.string/split findnode #" "))) (cal-graph-nodes (rest gn)))))
      )
    )

  ;; found all nodes in graph

  (def graphnodes (cal-graph-nodes nums))


  ;; 'cal-graph-edges' function to determine all the graphedges in the given graph problem

  (defn cal-graph-edges [ge]
    (def findnode (first ge))
    (if-not (empty? findnode)
      (flatten (cons (partition 2 (map #(Integer/parseInt %) (clojure.string/split findnode #" "))) (cal-graph-edges (rest ge))))
      )

    )

  ;; found all edges in graph

  (def graphedges (partition 2 (cal-graph-edges nums)))


  ;; 'adjacency' function to determine all adjacent members for each node in the graph

  (defn  adjacency [currentnode edges]
    (def firstedge (first edges))
    (def firstnode (first (first edges)))
    (def secondnode (second (first edges)))


    (if (and (integer? firstnode) (integer? secondnode))
      (if (or (= currentnode firstnode) (= currentnode secondnode))
        (cons firstedge (adjacency currentnode (rest edges)))
        (adjacency currentnode (rest edges))
        )
      )
    )



  ;; 'node-edge-relation' function is used to visit all neigbouring edges for a particular node which is useful for optimal graph coloring

  (defn node-edge-relation [allnodes alledges]
    (def mynode (first allnodes))
    (if (integer? mynode)
      (cons (adjacency mynode alledges) (node-edge-relation (rest allnodes) alledges))
      )
    )

  ;; getting all edges with a node

  (def allconnections (node-edge-relation graphnodes graphedges))


  ;; 'node-mapping' function maps all adjacent nodes for a particular node for better user understanding and also for information retrival

  (defn node-mapping [connections allnodes]
    (def current-node-map (first connections))
    (def nodevalue (first allnodes))
    (if (seq? current-node-map)
      (cons (cons nodevalue (remove #{nodevalue} (distinct (flatten current-node-map)))) (node-mapping (next connections)(rest allnodes)))
      {}
      )
    )


  (def mapnodes (node-mapping allconnections graphnodes))

  ;; 'showmapping' function keeps it in a map collection (key as 'currentnode' and value as 'all its neigbouring nodes')

  (defn showmapping [myconnections]
    (def firstmap (first myconnections))
    (if-not (empty? firstmap)
      (merge (hash-map (keyword (reduce #(clojure.string/replace-first %1 "?" %2) firstmap)) (into [] (rest firstmap))) (showmapping (next myconnections)))
      ()
      )
    )


  ;; mapping of each node with others in key value form

  (def graphmap (showmapping mapnodes))


  ;; 'min-adjacent-for-node' variable stores the particular graphnode which has least adjacent nodes connected to it so that we get maximum black color nodes for graph coloring problem

  (def min-adjacent-for-node (last (into (sorted-map-by (fn [key1 key2]
                                                          (compare [(count (get graphmap key2)) key2]
                                                                   [(count (get graphmap key1)) key1])))
                                         graphmap)))

  ;; 'parse-int' function converts the string input to integer

  (defn parse-int [s]
    (Integer. (re-find  #"\d+" s )))

  ;; 'guess-black-coloring-nodes' function is used as part of operation in below 'possible-blackcoloring' function to keep all possible black coloring nodes with its adjacent nodes in a map collection

  (defn guess-black-coloring-nodes [possible-color-nodes]
    (def mycolornode (first possible-color-nodes))
    (if (integer? mycolornode)
      (cons (cons mycolornode (graphmap (keyword (str mycolornode)))) (guess-black-coloring-nodes (rest possible-color-nodes)))
      )
    )

  ;; 'possible-blackcoloring' function tries to visit all nodes that are not adjacent to 'min-adjacent-for-node' variable for checking if the node can be black colored or not

  (defn possible-blackcoloring []
    (def n1 (remove (set (vector (parse-int (name (first min-adjacent-for-node))))) (set graphnodes)))

    ;; all connections of wanted nodes for black coloring

    (def nextlookup (remove (set (flatten (rest min-adjacent-for-node))) (set n1)))
    (guess-black-coloring-nodes nextlookup))

  ;; 'combinations' function is used to obtain all combinations for all the nodes we got from 'possible-blackcoloring' function so that we can perform mathematical operations (intersection, cross-mapping)

  (defn combinations [n coll]
    (if (= 1 n)
      (map list coll)
      (lazy-seq
        (when-let [[head & tail] (seq coll)]
          (concat (for [x (combinations (dec n) tail)]
                    (cons head x))
                  (combinations n tail))))))

  (def combo (possible-blackcoloring))

  ;; after node combinations, we have 'blackcolor-nodes' function to get our final optimal black color nodes for the given graph problem

  (defn blackcolor-nodes [combine]
    (def firstblack (first combine))
    (def secondblack (second combine))

    (cond

      ;; checking intersection and cross-mapping condition for coloring graph

      (and (or (some (set (vector (first firstblack))) (set secondblack)) (some (set (vector (first secondblack))) (set firstblack))) (some (set (rest firstblack)) (set (rest secondblack)))) (partition 2 (flatten (cons (list (parse-int (name (first min-adjacent-for-node))) (first firstblack)) (list (parse-int (name (first min-adjacent-for-node))) (first secondblack)))))

      ;; checking intersection condition for coloring graph

      (some (set (rest firstblack)) (set (rest secondblack))) (list (list (parse-int (name (first min-adjacent-for-node))) (first firstblack) (first secondblack)))

      )
    )

  ;; maximum-blacknodes function is used to determine maximum number of the nodes covered by black color and also the nodes that are covered by the black color which is our expected output

  (defn maximum-blacknodes [maxblack]
    (def resultnode (first maxblack))
    (if (empty? resultnode)
      ()
      (cons (blackcolor-nodes resultnode) (maximum-blacknodes (rest maxblack)))
      )
    )

  (def com (combinations 2 combo))

  ;; trying to keep our expected output in the approprite map collection format (retriving all black color nodes and number of black color nodes from map collection)

  (def myresult (if (= (count combo) 1) (list (cons (parse-int (name (first min-adjacent-for-node))) (list (first (flatten (combinations 1 combo)))))) (maximum-blacknodes com)))

  (def all-color-solutions (apply concat (remove nil? myresult)))

  (def sorted-order-solutions (if (and (integer? (first all-color-solutions)) (integer? (last all-color-solutions))) (sort-by count (vector all-color-solutions)) (sort-by count (concat all-color-solutions))))

  (def graph-coloring-solution (group-by count sorted-order-solutions))

  ;; printing the expected output as shown below

  (println "Maximum number of nodes covered by black color : " (key (last graph-coloring-solution)))

  (println "Nodes covered by black color : " (val (last graph-coloring-solution)))

  )


