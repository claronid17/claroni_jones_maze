(ns claroni-jones-maze.claroni_jones_maze)


;Authors: Dan Claroni and Rob Jones
;Our maze problem "claroni_jones_maze"
;want to solve a simple maze

;maze and gamestate are often interchangable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; Terminal, function, and primitive sets  and decided range for max-depth ;;;


;;;;;;;;;;;;;;;;;;;;; terminal sets and functions sets



(def terminal-set                                                     ;think about adding not of everying into terminal set
  '(wall-l wall-r wall-u wall-d  
           not-wall-l not-wall-r not-wall-u not-wall-d
           finish-l finish-r finish-u finish-d
           breadcrumb-r breadcrumb-d breadcrumb-l breadcrumb-u
           last-move))
(defn rand-term
  "returns a ranom value in the terminal set"
  []
  (rand-nth terminal-set))



(defn andd
  "Our own and function that takes 3 arguments and returns the first vector that has false as its first item
   or the last vector that has true as the first item"
  [arg1 arg2 arg3]
  (let [bool1 (first arg1)
        bool2 (first arg2)
        bool3 (first arg3 )]
    (cond
      (= bool1 false) arg1
      (= bool2 false) arg2
      (= bool3 false) arg3
      :else arg3)))



(defn randd
  "Our own randd function that takes 3 arguments and returns a random of the 3"
  [arg1 arg2 arg3]
  (rand-nth (vector arg1 arg2 arg3)))
;tester
(randd 'wall-l 'wall-r 'wall-d)



(defn orr
  "Our own or function that takes 3 arguments and returns the first vector that has true as its first item
   or the last vector that has false as its first item"
  [arg1 arg2 arg3]
  (let [bool1 (first arg1)
        bool2 (first arg2)
        bool3 (first arg3 )]
    (cond
      (= bool1 true) arg1
      (= bool2 true) arg2
      (= bool3 true) arg3
      :else arg3)))


(defn iff
  "Our own if function that takes 3 argument.  Each of the arguments is a vector.
   Within the vector are a boolean and a keyword.  The iff will check if the first 
   arguments boolean is a true or false.  If it is true, it will return the second 
   argument.  If it is false, it will return the second argument"
  [arg1 arg2 arg3]
  (let [bool1 (first arg1)]
    (if bool1
      arg2
      arg3)))





(def function-set
  '(iff andd orr))
(defn rand-fn
  "returns a ranom value in the function set"
  []
  (rand-nth function-set))


(def primitive-set
  (concat terminal-set function-set))
(defn rand-prim
  "returns a ranom value in the primitive set"
  []
  (rand-nth primitive-set))


(defn depth-range 
  "Returns a number from range 2-3"
  []
  (rand-nth (range 2 4))) 







; maze and helpers

(def default-maze
  '[  [| | | | | | | | | |]
      [| | | | | | | | | |]
      [| | | | | | | | | |]
      [| | | | | | | | | |]
      [| | | | | | | | | |]
      [| | | | | | | | | |]  ])

(def maze1 
  '[  [| | | | | | | | | |]  
      [| * _ _ _ _ _ _ F |] 
      [| | | | | | | | | |]])
(def maze2 
  '[ [| | | | | | | | | |]  
      [| F _ _ _ _ _ _ * |] 
      [| | | | | | | | | |]])

(def maze3
  '[  [| | | | | | | | | |]
      [| * _ _ _ _ _ _ | |]
      [| | | | | | | _ | |]
      [| | | | | | | _ | |]
      [| | | | | | | F | |]
      [| | | | | | | | | |]])
(def maze4
  '[  [| | | | | | | | | |]
      [| F _ _ _ _ _ _ | |]
      [| | | | | | | _ | |]
      [| | | | | | | _ | |]
      [| | | | | | | * | |]
      [| | | | | | | | | |]])

(def maze5
  '[ [| | | | | | | | | |]
      [| * _ _ | | _ _ F |]
      [| | | _ | | _ | | |]
      [| | | _ | | _ | | |]
      [| | | _ _ _ _ | | |]
      [| | | | | | | | | |]])
(def maze6
  '[ [| | | | | | | | | |]
     [| F _ _ | | _ _ * |]
     [| | | _ | | _ | | |]
     [| | | _ | | _ | | |]
     [| | | _ _ _ _ | | |]
     [| | | | | | | | | |] ])
(def maze7
  '[  [| | | | | | | | | |]
      [| F _ _ | | _ _ _ |]
      [| | | _ | | _ | | |]
      [| | | _ | | * | | |]
      [| | | _ _ _ _ | | |]
      [| | | | | | | | | |]  ])

(def maze8
  '[  [| | | | | | | | | |]
      [| | | | | | F | | |]
      [| | | | | | _ | | |]
      [| | | | | | _ | | |]
      [| * _ _ _ _ _ | | |]
      [| | | | | | | | | |]  ])

(def maze9
  '[  [| | | | | | | | | |]
      [| | * | | | F | | |]
      [| | _ | | | _ | | |]
      [| | _ | | | _ | | |]
      [| | _ _ _ _ _ | | |]
      [| | | | | | | | | |]  ])

(def maze10
  '[  [| | | | | | | | | |]
      [| * _ _ | | _ _ _ |]
      [| | | _ | | _ | _ |]
      [| | | _ | | _ | _ |]
      [| | | _ _ _ _ | _ |]
      [| | | | | | | | _ |]
      [| | | | | F _ _ _ |]
      [| | | | | | | | | |]])

(def maze11
  '[  [| | | | | | | | | |]
      [| * _ _ _ _ _ _ _ |]
      [| _ | _ _ | | _ | |]
      [| _ _ _ | | | _ _ |]
      [| | _ | | | | | _ |]
      [| | _ _ _ F | | | |]
      [| | | | | | | | | |]])

(def maze12
  '[  [| | | | | | | | | | | | | |]
      [| _ _ * _ _ _ _ _ _ _ _ _ |]
      [| _ | _ | | | | | | | _ | |]
      [| _ | _ | | | | _ _ _ _ | |]
      [| _ _ _ _ _ _ _ | _ | | | |]
      [| _ | | | _ | _ _ _ _ _ | |]
      [| _ _ | | _ F _ | | | _ | |]
      [| | | | | | | | | | | | | |] ])

(def maze13
  '[  [| | | | | | | | | | | | | |]
      [| _ _ * _ _ _ _ _ _ _ _ _ |]
      [| _ | _ | | | | | | | _ | |]
      [| _ | _ | | | | _ _ _ _ | |]
      [| _ _ _ _ _ _ _ | _ | | | |]
      [| _ | | | _ | _ _ _ _ _ | |]
      [| _ _ | F _ _ _ | | | _ | |]
      [| | | | | | | | | | | | | |] ])







(defn abs
  "Absolute value of x"
  [x]
  (max x (- 0 x)))



(defn abs-list-subtraction
  "Subtracts the values of two lists and returns a list of the absolute value
   of the subtraction"
  [list1 list2]
  (loop [i (- (count list1) 1)
         lst '()]
    (if (< i 0)
      lst
      (recur (dec i)
             (conj lst (abs (- (nth list1 i) (nth list2 i))))
             )
      )
    )
  )

(defn list-subtraction
  "Subtracts the values of two lists and returns a list of the absolute value
   of the subtraction"
  [list1 list2]
  (loop [i (- (count list1) 1)
         lst '()]
    (if (< i 0)
      lst
      (recur (dec i)
             (conj lst (- (nth list1 i) (nth list2 i)))
             )
      )
    )
  )


(defn get-maze-symbol
  [maze pos-vec]
  (nth (nth maze (first pos-vec)) (second pos-vec)))
;tester
(get-maze-symbol maze1 [0 3])



(defn get-player
  "Returns coordinates of the player in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'*} (nth maze layer)) '*)
      [layer (.indexOf (nth maze layer) '*)] ;[column row]
      (recur (inc layer)))))
;tester
(get-player maze2)



(defn get-finish
  "Returns coordinates of the finish in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'F} (nth maze layer)) 'F)
      [layer (.indexOf (nth maze layer) 'F)] ;[column row]
      (recur (inc layer)))))
;tester
(get-finish maze1)

        
  



(defn check-for-wall
  "Checks if there is a wall in the direction of the attempted move. 
   Returns true if there is a wall in the move-direction and false if there is not"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) [(= '| (get-maze-symbol maze [(- player-column 1) player-row])) :U]
      (= move :D) [(= '| (get-maze-symbol maze [(+ player-column 1) player-row])) :D]
      (= move :L) [(= '| (get-maze-symbol maze [player-column (- player-row 1)])) :L]
      (= move :R) [(= '| (get-maze-symbol maze [player-column (+ player-row 1)])) :R]
      )))
;tester
(check-for-wall maze7 :D)
(get-player maze7)
(list-subtraction (get-finish maze1) (get-player maze1))



(defn get-distance-to-finish                                                                
  "Calculates the absolute distance of the player to the finish."
  [maze]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))                                        
        finish-column (first (get-finish maze))
        finish-row (second (get-finish maze))
        minDist (Math/pow (+ (Math/pow (- finish-column player-column) 2) (Math/pow (- finish-row player-row) 2)) 0.5)] ;basic distance formula d= sqrt( (x2-x1)^2 + (y2-y1)^2)
    (cond
      (= minDist (float (- finish-row player-row))) 
      (if (check-for-wall maze :D) 
        (+ 20 minDist) 
        minDist)   ;if there is a wall between the player and the finish add 20 to the minDistance returned
      
      (= minDist (float (- player-row finish-row))) 
      (if (check-for-wall maze :U) 
        (+ 20 minDist) 
        minDist)
      
      (= minDist (float (- finish-column player-column))) 
      (if (check-for-wall maze :R) 
        (+ 20 minDist) 
        minDist)
      
      (= minDist (float(- player-column finish-column))) 
      (if (check-for-wall maze :L) 
        (+ 20 minDist) 
        minDist)
      
      :else minDist
      )        
    ))    
;tester.
(def temp 
'[[| | | | | | | | | |]
  [| = = = | | _ _ _ |]
  [| | | = | | _ | _ |]
  [| | | = | | _ | _ |]
  [| | | = = * _ | _ |]
  [| | | | | | | | _ |]
  [| | | | | F _ _ _ |]
  [| | | | | | | | | |]])
temp
(get-distance-to-finish temp)



(defn not-check-for-wall
  "Checks if there is a wall in the direction of the attempted move. 
   Returns true if there is a wall in the move-direction and false if there is not"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) [(not (= '| (get-maze-symbol maze [(- player-column 1) player-row]))) :U]
      (= move :D) [(not (= '| (get-maze-symbol maze [(+ player-column 1) player-row]))) :D]
      (= move :L) [(not (= '| (get-maze-symbol maze [player-column (- player-row 1)]))) :L]
      (= move :R) [(not (= '| (get-maze-symbol maze [player-column (+ player-row 1)]))) :R]
      )))
;tester
(not-check-for-wall maze1 :R)





(defn check-for-breadcrumb
  "Checks if there is a breadcrumb ('=) in the direction of the attempted move. 
   Returns true and the opposite direction if there is a breadcrumb in the move-direction and false and the move direction if there is not"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) (let [bool (= '= (get-maze-symbol maze [(- player-column 1) player-row]))]
                    [bool (if (= bool true)
                            :D
                            :U)])
      
      (= move :D) (let [bool (= '= (get-maze-symbol maze [(+ player-column 1) player-row]))]
                        [bool (if (= bool true)
                            :U
                            :D)])
      (= move :L) (let [bool (= '= (get-maze-symbol maze [player-column (- player-row 1)]))]
                        [bool (if (= bool true)
                            :R
                            :L)])
      (= move :R) (let [bool (= '= (get-maze-symbol maze [player-column (+ player-row 1)]))]
                    [bool (if (= bool true)
                            :L
                            :R)])
      )))
;tester




(defn check-for-finish                                                                
  "Checks if the finish direction is in the move direction. 
   Returns true if the finish IS  in the move direction
   Returns false if the finish IS NOT in the move direction"
  [maze move]
  (let [distance (list-subtraction (get-finish maze) (get-player maze))
        up-dist (first distance)
        r-dist (second distance)
        ]
    (cond
      (= move :U) [(and (> up-dist 0) (> up-dist (abs r-dist))) :U]
      (= move :D) [(and (< up-dist 0) (> (abs up-dist) (abs r-dist))) :D]
      (= move :R) [(and (> r-dist 0) (> r-dist (abs up-dist))) :R]
      (= move :L) [(and (< r-dist 0) (> (abs r-dist) (abs up-dist))) :L]
      )))
;tester
maze1
(check-for-finish maze1 :R)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;                MOVE PLAYER START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn switch-rows
  "moves character between rows and leaves breadcrumbs."
  [source destination col]
  (loop [new-source []
         new-destination []
         index 0]
    (cond 
      (= index (count source)) [new-source new-destination]
      (= index col) (recur (conj new-source '=) (conj new-destination '*) (inc index))
      :else (recur (conj new-source (nth source index)) 
                   (conj new-destination (nth destination index)) 
                   (inc index)))))



(defn switch-columns
  "moves character to left or right and leaves breadcrumbs"
  [row character destination]
  (loop [new-row []
         index 0]
    (cond
      (= index (count row)) new-row
      (= index (min character destination)) (if (= (min character destination) character)
                                              (recur (conj new-row '= '*) (+ index 2))
                                              (recur (conj new-row '* '=) (+ index 2)))
      :else (recur (conj new-row (nth row index)) (inc index)))))





(defn move-player
  [maze move]
  (let [player-column (second (get-player maze))
        player-row (first (get-player maze))]
    (cond
      (= move :U)
      (if  (first (check-for-wall maze :U))
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
      (if (first (check-for-wall maze :D))
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
      (if (first (check-for-wall maze :L))
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
      (if (first (check-for-wall maze :R))
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
(move-player (move-player maze1 :R) :R)
(move-player maze7 :D)


  
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;                            MOVE PLAYER END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; evaluating programs


(defn program-to-fn                                                  ;this  will turn the prog above into an actual function using eval
  "Takes a GP program represented as a list, with inputs wall-l 
   wall-r wall-u wall-d finish-l finish-r finish-u finish-d,                  
   and transforms it into a function that can be called on an input."
  [program]                                                                
  (eval (list 'fn                                                          ; is basiccally the same thing as doung (fn [x] program))
              '[wall-l
                wall-r
                wall-u
                wall-d
                not-wall-l
                not-wall-r
                not-wall-u
                not-wall-d
                finish-l
                finish-r
                finish-u
                finish-d
                breadcrumb-r
                breadcrumb-d
                breadcrumb-l
                breadcrumb-u
                last-move]
              program)))


(defn evaluate 
  "Evaluates a program with a given x-value.
   With 3 inputs, the first one should be map, second should be the program and
   the thrid should be the desired range to map the function along.
   With 4 inputs, the first is the map function, second is the program, third is the 
   start of the range (inclusive), fourth is the end of the range function (exclusive)"
  ([program gamestate last-move]   ; wall-l-check will return a vector (and all others will too
    (let [prog-fn (program-to-fn program)]
      (prog-fn (check-for-wall gamestate :L)
               (check-for-wall gamestate :R)
               (check-for-wall gamestate :U)
               (check-for-wall gamestate :D)
               (not-check-for-wall gamestate :L)
               (not-check-for-wall gamestate :R)
               (not-check-for-wall gamestate :U)
               (not-check-for-wall gamestate :D)
               (check-for-finish gamestate :L)
               (check-for-finish gamestate :R)
               (check-for-finish gamestate :U)
               (check-for-finish gamestate :D)
               (check-for-breadcrumb gamestate :R)
               (check-for-breadcrumb gamestate :D)
               (check-for-breadcrumb gamestate :L)
               (check-for-breadcrumb gamestate :U)
               [true last-move])
      )
    )
  )
;tester
(evaluate 'last-move maze1 :L)


(defn perform-program
  "Returns the move (second object in the vector) returned by evaluate"
  [program gamestate last-move]
  (second (evaluate program gamestate last-move)))
;tester
maze1
(perform-program '(andd wall-r wall-d wall-d) (move-player maze1 :R) :R)
(perform-program 'wall-l (move-player maze1 :R) :R)




(defn print-maze
  "Prints the maze in the right order and allignment"
  [maze]
  (loop [maze-str nil
         first-m (first maze)
         rest-m (rest maze)]
    (if (empty? rest-m)
      (println (str maze-str first-m "\n"))
      (recur
        (str maze-str first-m "\n")
        (first rest-m)
        (rest rest-m)
        )
      )
    )
  )
;tester
(print-maze maze5)
    



(defn next-legal-move
  "Generates the next legal move by checking for walls in the maze."
  [maze move]
  (let [player-column (second (get-player maze))
        player-row (first (get-player maze))]
    (cond
      (= move :U)
      (if  (first (check-for-wall maze :U))
        (next-legal-move maze :R)
        :U)
      (= move :R)
      (if  (first (check-for-wall maze :R))
        (next-legal-move maze :D)
        :R)
      (= move :D)
      (if  (first (check-for-wall maze :D))
        (next-legal-move maze :L)
        :D)
      (= move :L)
      (if  (first (check-for-wall maze :L))
        (next-legal-move maze :U)
        :L))
    )
  )
;tester
(move-player (move-player maze5 :R) :R)
(next-legal-move (move-player (move-player maze5 :R) :R) :U)
(next-legal-move (move-player maze1 :R) :L)
(check-for-wall maze1 :R)



(defn state-steps
  "Prints the state of a maze after each move that a program produces"
  [maze program]
  (let [finish (get-finish maze)]
    (loop [gamestate maze
           moves 0
           last-move :R]
      (print-maze gamestate)
      (let [player (get-player gamestate)]
        (cond
          (= finish player) (println (str "###########################################\n" (str "Maze completed in " moves " moves!")))
          (>= moves 50) (str "##################################\n\nMax number of moves reached: " moves)
          :else (let [move (next-legal-move gamestate (perform-program program gamestate last-move))]                           
            
            (recur 
              (move-player gamestate move)
              (inc moves)
              move
              )
            )
          )
        )
      )
    )
  )
;tester
(state-steps maze1 'wall-l)
(state-steps maze3 'wall-l)
(state-steps maze5 'last-move)



         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;  Initialization methods and helper functions  ;;;;;;;;;;;;;;;;;;;;;;;;


(defn full                                        ; yay!!! full works
  "Builds a function using Full method"
  [max-d]
  (cond
    (= max-d 0) (rand-term)                              ;terminal if leave
    (= max-d 1) (list (rand-fn) (rand-term)(rand-term)(rand-term))   ; function with two terminals if 1 up from frontier
    :else (list (rand-fn) (full (dec max-d)) (full (dec max-d))(full (dec max-d)))  ;recursion otherwise
    )
  )



(defn grow                                                                   ; yay! grow works
  "Builds a function using Grow method, by returning one value at a time"
  [max-d]
  (let [d max-d
        odds (rand)]
    (cond
      (= d 0) (rand-term)
      (= d 1) (if (< odds 0.5)
                (list (rand-fn) (rand-term) (rand-term) (rand-term))
                (rand-term))
      :else (if (< odds 0.5)
              (list (rand-fn) (grow (dec d)) (grow (dec d)) (grow (dec d)))
              (rand-term))
      )
    )
  )



(defn ramped-h-h                                              
  "Builds a program tree using ramped half and half"
  []
  (let [odds (rand)]
    (if (< odds 0.5)
      (grow (depth-range))
      (full (depth-range)))
    )
  )


(defn generate-init-population
  [pop-size]
  (take pop-size (repeatedly ramped-h-h)))               ;we used ramped-h-h to help vary the functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fitness functions
;might need to edit this to optimize runs

(defn program-fitness                                       ; fitness is shortest distance from finish *10 + # of moves >29 .. basically a simulator with just state
  "Returns the fitness of a program.
   Fitness is based calculated by the equation:
   Shortest distance from the maze's finish * 10 + number of moves after 15 moves"
  [program maze]
   (let [finish (get-finish maze)]
    (loop [gamestate maze
           total-moves 0
           moves>15 0
           shortest-distance (get-distance-to-finish gamestate)
           last-move :R]
      
      (let [player (get-player gamestate)]
        (cond
          (= finish player) moves>15
          (>= total-moves 30) (+ (* (min shortest-distance (get-distance-to-finish gamestate)) 10) moves>15)             ;min shortest moves and current position
          :else (let [move (next-legal-move gamestate (perform-program program gamestate last-move))]                                                          ;30 moves used to calculate fitness but 50 when visualizing the moves          
            
                 (recur 
                   (move-player gamestate move)
                   (inc total-moves)
                   (if (>= total-moves 15)
                     (inc moves>15)                         ;only increments the moves>20 (essentially fitness) if we have already made 20 moves
                     moves>15)
                   (min shortest-distance (get-distance-to-finish gamestate))
                   move
                   )
                 )
          )
        )
      )
    )
  )
;tester
(def rand-prog (grow 2))
rand-prog
(state-steps maze5 rand-prog)
(program-fitness rand-prog maze5)


(defn population-fitness                                                  ;mostly helpful for visualization of best functions
  "Returns a list of the fitness of each individual population"
  [population maze]
  (map (fn [x] (program-fitness x maze)) population))
;tester
(population-fitness (generate-init-population 50) maze5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;parent selection


(defn threshold-selection                       ;could use this functionn to reduce the population to 
  ;semi-decent programs and then do tourni and such
  "Returns a list of the individual programs in the population that have a fitness below or equal to the given
   fitness value"
  [value prog-pop maze]
  (filter (fn [x] (<= (program-fitness x maze) value)) prog-pop))   ;we found that this function greatly reduces diversity in population, 
;so we didnt include it in our genetic-programming fn but wanted it to
;be in here in-case we needed it for the next project
;tester
(threshold-selection 50 (generate-init-population 10) maze5)


(defn tournament-selection 
  "Randomly selects 3 programs from the population input and outputs the best one with the best fitness"
  [prog-pop maze]
  (loop [i 0
         lst '()]
    (if (not (>= i 3))
      (recur (inc i)
             (conj lst (rand-nth prog-pop)))
      (first (sort-by (fn [x] (program-fitness x maze)) lst)))))
;tester
(tournament-selection (generate-init-population 10) maze1)


(defn best-n-progs                          ;this helps us guide our evolution towards better programs and also sorts programs 
  "Returns the best n number of programs from the given program population"
  [prog-pop n maze]
  (take n (sort-by (fn [x] (program-fitness x maze)) prog-pop)))
;tester
(def besties (best-n-progs (generate-init-population 20) 10 maze5))
besties
(population-fitness besties maze5)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;mutations helpers


(def instructions
  '{andd 2                               ; ' mark is important because it allows each to be a symbol
    orr 2
    iff 2})


(defn program-size
  "Finds the size of the program, i.e. number of nodes in its tree."
  [prog]
  (if (not (seq? prog))            ; if its not a sequence it will return 1.. this lets us evalute the size of a terminal
    1
    (count (flatten prog))))       ;flatten essentially removes all parenthesis so you can easily evaluate size of the tree


(defn select-random-subtree                                         
  "Given a program, selects a random subtree and returns it."         ;prof helmuths code 
  ([prog]
    (select-random-subtree prog (rand-int (program-size prog))))
  ([prog subtree-index]
    (cond
      (not (seq? prog)) prog
      (and (zero? subtree-index)
           (some #{(first prog)} (keys instructions))) prog
      (< subtree-index (program-size (first prog))) (recur (first prog)
                                                           subtree-index)
      :else (recur (rest prog)
                   (- subtree-index (program-size (first prog)))))))
;tester
(select-random-subtree (full 3))


(defn replace-random-subtree                                                                   ; basically pt mutation
  "Given a program and a replacement-subtree, replace a random node       
   in the program with the replacement-subtree."
  ([prog replacement-subtree]
    (replace-random-subtree prog replacement-subtree (rand-int (program-size prog))))        ;prof helmuths code
  ([prog replacement-subtree subtree-index]
    (cond
      (not (seq? prog)) replacement-subtree
      (zero? subtree-index) replacement-subtree
      :else (map (fn [element start-index]
                   (if (<= start-index
                           subtree-index
                           (+ start-index -1 (program-size element)))
                     (replace-random-subtree element
                                             replacement-subtree
                                             (- subtree-index start-index))
                     element))
                 prog
                 (cons 0 (reductions + (map program-size prog)))))))
;tester
(replace-random-subtree (full 2) 'dingly-dongly-doo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mutation or variation


(defn replication                                  ;basically reproduction
  "Returns clone of the input program."
  [prog]
  prog)


(defn subtree-mutation
  "Selects a random node from an input program and replaces it with a random subtree
   of max-depth 3"
  [program]
  (replace-random-subtree program (grow 3)))    ;arbitrarily selected max-depth 3
;tester
(def proggi (full 2))
proggi
(subtree-mutation proggi)


(defn hoist-mutation                           ;helps with our bloat problem
  "Selects root-node from the input program and replaces it with a random subtree
   from the program"
  [program]
  (replace-random-subtree program (select-random-subtree program) 0))
;tester
proggi
(hoist-mutation proggi)


(defn pt-mutation
  "Selects a random node in a program and replaces it with a new node of the same type"
  [prog]
  (let [node (rand-int (program-size prog))              ;selects random number in prog size
        sub-tree (select-random-subtree prog node)]       ;selects the subtree at that number
    (if (seq? sub-tree)
      (replace-random-subtree prog (conj (rest sub-tree) (rand-fn)) node) ;replace with a random fn if a fn
      (replace-random-subtree prog (rand-term) node) ;replace with rand terminal if terminal
      )
    ))
;tester
proggi
(pt-mutation proggi)


(defn cross-over
  "Performs cross-over mutation on two input parents
   by replacing a random subtree from parent 1 with a
   random subtree in parent2"
  [parent1 parent2]
  (replace-random-subtree parent1 (select-random-subtree parent2)))
;tester
(cross-over proggi proggi)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;mutate an entire generation

(defn mutate-generation
  "Takes an entire population of programs and produces a population of mutated children.
   Parameters: pt-mutation: 10%
               subtree-mutation: 10%
               cross-over: 55%
               hoist-mutation: 20%
               random-program: 3%
               replication: 2%"
  [population maze]
  
  (let [pop population]
    (loop [n (rand-int 100)
           next-gen '()
           count 30]                               ;produces 30 children every time
      
      (if (= 0 count)
        next-gen
        (cond
          (< n 10) (recur  ;10% point mutation on a random program selected via tournament selection
                           
                           (rand-int 100)
                           (conj next-gen (pt-mutation (tournament-selection pop maze)))
                           (dec count))
          (< n 20) (recur ;10% sub-tree mutation on a random program selected via tournament selection
                          
                          (rand-int 100)
                          (conj next-gen (subtree-mutation (tournament-selection pop maze)))
                          (dec count))
          (< n 75) (recur ;55% cross over on two random programs selected via tournament selection
                          
                          (rand-int 100)
                          (conj next-gen (cross-over (tournament-selection pop maze) (tournament-selection pop maze)))
                          (dec count))
          (< n 95) (recur ;20% hoist mutation on a random program selected via tourny selection 
                          
                          (rand-int 100)
                          (conj next-gen (hoist-mutation (tournament-selection pop maze)))
                          (dec count))
          (< n 98) (recur ;3% ramped-h-h 
                          
                          (rand-int 100)
                          (conj next-gen (ramped-h-h))
                          (dec count))
          (< n 100) (recur ;2% replication
                           
                           (rand-int 100)
                           (conj next-gen (replication (tournament-selection pop maze)))
                           (dec count))
          )
        )
      )
    )
  )
;tester
(def rand-gen (generate-init-population 3))
rand-gen
(mutate-generation rand-gen maze5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;main

(defn genetic-programming
  "This function only which maze you want to test the genetic programming on.
   It generates an initial population size of 30, and produces
   new generations via different methods of mutation of population size 30.
   Each generation prints the generation number, the best program
   in that generation and the total error of that program.
   Will terminate at 50 generations or when a solution is found."
  [maze]
  (loop [pop (generate-init-population 30)          
         gen-number 1]
    (let [best-20 (best-n-progs pop 20 maze)    ;takes the best 20 programs in order of fitness to move evolution towards better programs
          pop-fitness (apply + (population-fitness best-20 maze))
          best-prog (first best-20)             ; since best20 is sorted we can take first for best
          best-err (float(program-fitness best-prog maze))]    
      (println "Generation number: " gen-number)
      (println "Best program this generation:" best-prog)
      (println "Total error of that program:"  best-err)
      (println "\nBest 20 Programs Total Fitness:"  pop-fitness)
      (println "\n############################################################\n")
      (cond 
        (<= best-err 15) (println "*****Solution Found!*****\n Solution is:" best-prog "\n")
        (= gen-number 50) (println "Max generations reached (" gen-number
                                   ").\nBest program:" best-prog
                                   "\nIt's error:" best-err 
                                   "\n\n############################################################\n")
        :else (recur
                (mutate-generation best-20 maze)
                (inc gen-number))
        )
      )
    )
  )





