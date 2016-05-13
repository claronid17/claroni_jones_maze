(ns robert-jones.simulator)
(def maze1
  '(
     [| | | | | | | | | |]
     [| * _ _ _ _ _ _ F |]
     [| | | | | | | | | |]))

(def maze2
  '(
     [| | | | | | | | | |]
     [| _ * _ _ _ _ _ F |]
     [| | | | | | | | | |]))

(defn get-maze-symbol
  "Returns the item in the maze at the row, column location."
  [maze row column]
  (nth (nth maze row) column))

(defn get-maze-symbol
  [maze pos-vec]
  (nth (nth maze (first pos-vec)) (second pos-vec)))

(defn get-player
  "Returns coordinates of the player in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'*} (nth maze layer)) '*)
      [(.indexOf (nth maze layer) '*) layer] ;[column row]
      (recur (inc layer)))))

(defn check-for-wall
  "Checks if there is a wall in the direction of the attempted move."
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) (not (= '| (get-maze-symbol maze [(- player-column 1) player-row])))
      (= move :D) (not (= '| (get-maze-symbol maze [(+ player-column 1) player-row])))
      (= move :L) (not (= '| (get-maze-symbol maze [player-column (- player-row 1)])))
      (= move :R) (not (= '| (get-maze-symbol maze [player-column (+ player-row 1)])))
      )))

(defn switch-rows
  "moves character between rows."
  [source destination col]
  (loop [new-source []
         new-destination []
         index 0]
    (cond 
      (= index (count source)) [new-source new-destination]
      (= index col) (recur (conj new-source '_) (conj new-destination '*) (inc index))
      :else (recur (conj new-source (nth source index)) 
                   (conj new-destination (nth destination index)) 
                   (inc index)))))

(defn switch-columns
  "moves character to left or right"
  [row character destination]
  (loop [new-row []
         index 0]
    (cond
      (= index (count row)) new-row
      (= index (min character destination)) (if (= (min character destination) character)
                                              (recur (conj new-row '_ '*) (+ index 2))
                                              (recur (conj new-row '* '_) (+ index 2)))
      :else (recur (conj new-row (nth row index)) (inc index)))))


(defn move-player
  [maze move]
  (let [player-row (second (get-player maze))
        player-column (first (get-player maze))]
    (cond
      (= move :U)
      (if (not (first (check-for-wall maze :U)))
        (move-player maze :R)
        (loop [new-maze []
               row 0]
          (cond
            (= row (count maze))
            new-maze
            (= row (- player-row 1))
            (let [new-rows (switch-rows (nth maze player-row) (nth maze (- player-row 1)) player-column)]
              (recur (into [] (concat new-maze (reverse new-rows))) (+ row 2)))
            :else (recur (conj new-maze (nth maze row)) (inc row))))
        )
      (= move :D)
      (if (not (first (check-for-wall maze :D)))
        (move-player maze :L)
        (loop [new-maze []
               row 0]
          (cond
            (= row (count maze))
            new-maze
            (= row player-row)
            (let [new-rows (switch-rows (nth maze player-row) (nth maze (+ player-row 1)) player-column)]
              (recur (into [] (concat new-maze new-rows)) (+ row 2)))
            :else (recur (conj new-maze (nth maze row)) (inc row))))
        )
      (= move :L)
      (if (not (first (check-for-wall maze :L)))
        (move-player maze :U)
        (loop [new-maze []
               row 0]
          (cond
            (= row (count maze)) new-maze
            (= row player-row) (let [new-row (switch-columns (nth maze player-row) player-column (- player-column 1))]
                                 (recur (conj new-maze new-row) (inc row)))
            :else (recur (conj new-maze (nth maze row)) (inc row))))
        )
      (= move :R)
      (if (not (first (check-for-wall maze :R)))
        (move-player maze :D)
        (loop [new-maze []
               row 0]
          (cond
            (= row (count maze)) new-maze
            (= row player-row) (let [new-row (switch-columns (nth maze player-row) player-column (+ player-column 1))]
                                 (recur (conj new-maze new-row) (inc row)))
            :else (recur (conj new-maze (nth maze row)) (inc row))))
        ))))

                    


;tester
(get-maze-symbol maze1 1 1)
(get-player maze1)
