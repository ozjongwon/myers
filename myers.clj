;;;
;;; This is LLM generated code(95%) of git@github.com:ozjongwon/myers.git
;;;

(defn- get-v
  "Get the x coordinate for diagonal k from v-map"
  [v-map k max-d]
  (get v-map (+ k max-d) -1))


(defn- build-operations
  "Convert xy-distance-list into a sequence of edit operations.
   Returns :match if strings are identical."
  [xy-distance-list]
  (if (not (next xy-distance-list))
    :match
    (loop [[[prev-x prev-y] xy-list & next-xy-distance-list] xy-distance-list
           op-list ()]
      (if (nil? xy-list)
        op-list
        (let [[ins del skip] [[prev-x (dec prev-y)] [(dec prev-x) prev-y] [(dec prev-x) (dec prev-y)]]]
          (cond (some #(= % ins) xy-list) (recur `[~ins ~@next-xy-distance-list]
                                                 (cons (cons :insert ins) op-list))
                (some #(= % del) xy-list) (recur `[~del ~@next-xy-distance-list]
                                                 (cons (cons :delete del) op-list))
                :else (recur `[~skip ~xy-list ~@next-xy-distance-list]
                             (cons (cons :keep skip) op-list))))))))

(defn myers-distance
  [s1 s2]
  (let [n (count s1)
        m (count s2)
        max-d (+ n m)
        initial-v {1 0}  ; Start at coordinate (0,0)

        snake-move
        (fn [x y]
          (loop [x x
                 y y]
            (if (and (< x n)
                     (< y m)
                     (= (get s1 x) (get s2 y)))
              (recur (inc x) (inc y))
              [x y])))

        process-k
        (fn [d k v-map xy-list]
          (let [down-or-right (if (or (zero? (+ d k))
                                      (and (not= d k)
                                           (< (get-v v-map (dec k) max-d)
                                              (get-v v-map (inc k) max-d))))
                                (get-v v-map (inc k) max-d)
                                (inc (get-v v-map (dec k) max-d)))
                y (- down-or-right k)
                [final-x final-y] (snake-move down-or-right y)]
            {:v-map (assoc v-map (+ k max-d) final-x)
             :xy-list (cons [final-x final-y] xy-list)
             :found? (and (>= final-x n) (>= final-y m))}))

        process-d
        (fn [d v-map xy-history]
          (loop [k (- d)
                 v-map v-map
                 xy-list []]
            (if (> k d)
              {:v-map v-map
               :xy-history (cons xy-list xy-history)
               :found? false}
              (let [{:keys [v-map xy-list found?]}
                    (process-k d k v-map xy-list)]
                (if found?
                  {:xy-history (cons xy-list xy-history)
                   :found? true}
                  (recur (+ k 2) v-map xy-list))))))]

    (loop [d 0
           v-map initial-v
           xy-history []]
      (if (> d max-d)
        :fail
        (let [{:keys [v-map xy-history found?]}
              (process-d d v-map xy-history)]
          (if found?
            (build-operations (cons (ffirst xy-history) (rest xy-history)))
            (recur (inc d) v-map xy-history)))))))
