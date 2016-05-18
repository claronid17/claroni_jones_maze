(ns claroni-jones-maze.claroni_jones_maze)


;Authors: Dan Claroni and Rob Jones
;Our maze problem "claroni_jones_maze"
;want to solve a simple maze

;maze and gamestate are often interchangable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; Terminal, function sets  and decided range for max-depth ;;;


;;;;;;;;;;;;;;;;;;;;; terminal sets and functions sets



(def terminal-set                                                     
  '(wall-l wall-r wall-u wall-d  
           not-wall-l not-wall-r not-wall-u not-wall-d
           finish-l finish-r finish-u finish-d
           breadcrumb-r breadcrumb-d breadcrumb-l breadcrumb-u
           last-move))

(defn rand-term
  "returns a random value in the terminal set"
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
   Within the vector are a boolean and a direction keyword.  The iff will check if the first 
   arguments boolean is a true or false.  If it is true, it will return the second 
   argument.  If it is false, it will return the third argument"
  [arg1 arg2 arg3]
  (let [bool1 (first arg1)]
    (if bool1
      arg2
      arg3)))


(def function-set
  '(iff andd orr))

(defn rand-fn
  "returns a ranodm value in the function set"
  []
  (rand-nth function-set))


(defn depth-range 
  "Returns a number from range 2-3"
  []
  (rand-nth (range 2 4))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(def maze14
  '[  [| | | | | | | | | | | | | |]
      [| _ _ * _ _ _ _ _ _ _ _ _ |]
      [| _ | _ | | | | | | | _ | |]
      [| _ | _ | | | | _ _ _ _ | |]
      [| _ _ _ _ _ _ _ | _ | | | |]
      [| _ | | | _ | _ _ _ _ _ | |]
      [| _ | F | _ _ _ | | | _ | |]
      [| | | _ _ _ | | | | | | | |]
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
  "Subtracts the values of two lists and returns a list of the result
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
  "Returns the symbol within the maze at the 
   row, column pair in the position vector"
  [maze pos-vec]
  (nth (nth maze (first pos-vec)) (second pos-vec)))


(defn get-player
  "Returns vector coordinates of the player (*) in the maze."
  [maze]
  (loop [layer 0]
    (if (= (some #{'*} (nth maze layer)) '*)
      [layer (.indexOf (nth maze layer) '*)] 
      (recur (inc layer)))))


(defn get-finish
  "Returns vector coordinates of the finish (F) in the maze"
  [maze]
  (loop [layer 0]
    (if (= (some #{'F} (nth maze layer)) 'F) ;find f and
      [layer (.indexOf (nth maze layer) 'F)] ;return the coordinate
      (recur (inc layer)))))


(defn check-for-wall
  "Checks if there is a wall in the direction of the attempted move and returns a boolean-move vector. 
   Returns true as the first argument of the vector if there is a wall in the move-direction
   and false as the first argument of the vector if there is not as well as the move as the secodn
   argument --> example vector return [true :L]"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) [(= '| (get-maze-symbol maze [(- player-column 1) player-row])) :U] ;if move is up, check up and return true if wall or false if not and :U
      (= move :D) [(= '| (get-maze-symbol maze [(+ player-column 1) player-row])) :D]
      (= move :L) [(= '| (get-maze-symbol maze [player-column (- player-row 1)])) :L]
      (= move :R) [(= '| (get-maze-symbol maze [player-column (+ player-row 1)])) :R]
      )))


(defn get-distance-to-finish                                                                
  "Calculates the absolute distance of the player to the finish.  If the shortest
   distance has a wall between the player and the finish, the distance gets increased by 20."
  [maze]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))                                        
        finish-column (first (get-finish maze))
        finish-row (second (get-finish maze))
        minDist (Math/pow (+ (Math/pow (- finish-column player-column) 2) (Math/pow (- finish-row player-row) 2)) 0.5)] ;basic distance formula d= sqrt( (x2-x1)^2 + (y2-y1)^2)
    (cond
      (= minDist (float (- finish-row player-row))) ;if the min distance is purely down
      (if (check-for-wall maze :D)  ;check if there is a wall in the down direction
        (+ 20 minDist) ;if there is a wall between the player and the finish add 20 to the minDistance returned
        minDist)   ;if no wall, return the min distance
      
      (= minDist (float (- player-row finish-row))) ;same as above but for purely up
      (if (check-for-wall maze :U) 
        (+ 20 minDist) 
        minDist)
      
      (= minDist (float (- finish-column player-column)))  ;same as above but for purely right
      (if (check-for-wall maze :R) 
        (+ 20 minDist) 
        minDist)
      
      (= minDist (float(- player-column finish-column))) ;same as above but for purely left
      (if (check-for-wall maze :L) 
        (+ 20 minDist) 
        minDist)
      
      :else minDist ;if not purely a direction, just return min distance
      )        
    ))    


(defn not-check-for-wall
  "Checks if there is not a wall in the direction of the attempted move and returns a boolean-move vector. 
   Returns false as the first argument of the returned vectore if there is a wall
   in the move-direction and true as the first argument if there is not. The second
   argument is always the move... example vector returned [false :R]"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) [(not (= '| (get-maze-symbol maze [(- player-column 1) player-row]))) :U] ;simply notted check for wall
      (= move :D) [(not (= '| (get-maze-symbol maze [(+ player-column 1) player-row]))) :D]
      (= move :L) [(not (= '| (get-maze-symbol maze [player-column (- player-row 1)]))) :L]
      (= move :R) [(not (= '| (get-maze-symbol maze [player-column (+ player-row 1)]))) :R]
      )))


(defn check-for-breadcrumb
  "Checks if there is a breadcrumb ('=) in the direction of the attempted move and returns a boolean-move vector. 
   Returns a vector [true and the opposite direction of move] if there is a breadcrumb
   in the move-direction and [false and the move direction] if there is not"
  [maze move]
  (let [player-column (first (get-player maze))
        player-row (second (get-player maze))]
    (cond
      (= move :U) (let [bool (= '= (get-maze-symbol maze [(- player-column 1) player-row]))] ;checks for the = symbolaa
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


(defn check-for-finish                                                                
  "Checks if the finish direction is in the move direction and returns a boolean-move vector. 
   Returns true as the first argument of the vector if the finish IS  the furthest away
   in the move direction of returns false as the first argument if the
   finish IS NOT furthest away in the move direction. Second vector argument is always the move"
  [maze move]
  (let [distance (list-subtraction (get-finish maze) (get-player maze))  ;distance to finish
        up-dist (first distance) ;vertical distance to f
        r-dist (second distance) ;horz. distance to f
        ]
    (cond
      (= move :U) [(and (> up-dist 0) (> up-dist (abs r-dist))) :U] ;if finish is the furthest away in the U direction
      (= move :D) [(and (< up-dist 0) (> (abs up-dist) (abs r-dist))) :D]
      (= move :R) [(and (> r-dist 0) (> r-dist (abs up-dist))) :R]
      (= move :L) [(and (< r-dist 0) (> (abs r-dist) (abs up-dist))) :L]
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                MOVE PLAYER START



(defn switch-rows
  "Moves character between rows and leaves breadcrumbs."
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
  "Moves character to left or right and leaves breadcrumbs"
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
  "Moves the player in the maze and outputs a new maze or gamestate"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;evaluating programs


(defn program-to-fn                                                 
  "Takes a GP program represented as a list                 
   and transforms it into a function that can be called on an input."
  [program]                                                                
  (eval (list 'fn                                                          
              '[wall-l
                wall-r
                wall-u
                wall-d
                not-wall-l           ;all of these inputs correspond to a function call below in evaluate
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
  "Evaluates a program by calling functions for the symbols in the program tree."
  ([program gamestate last-move]   ; check-for-wall will return a boolean-move vector (and all others will too)
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


(defn perform-program
  "Returns the move (second object in the vector) returned by evaluate"
  [program gamestate last-move]
  (second (evaluate program gamestate last-move)))


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


(defn state-steps
  "Prints the state of a maze after each move that a program produces.
   Prints a message when the maze has been completed or when 50 moves 
   have been made."
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
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;  Initialization methods and helper functions  ;;;;;;;;;;;;;;;;;;;;;;;;


(defn full                                        
  "Builds a function using Full method"
  [max-d]
  (cond
    (= max-d 0) (rand-term)                              ;terminal if leave
    (= max-d 1) (list (rand-fn) (rand-term)(rand-term)(rand-term))   ; function with two terminals if 1 up from frontier
    :else (list (rand-fn) (full (dec max-d)) (full (dec max-d))(full (dec max-d)))  ;recursion otherwise
    )
  )


(defn grow                                                                   
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
  (take pop-size (repeatedly ramped-h-h)))              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fitness functions

(defn program-fitness                                       ; fitness is shortest distance from finish *10 + # of moves >29 .. basically a simulator with just state
  "Returns the fitness of a program.
   Fitness is calculated by the equation:
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
          :else (let [move (next-legal-move gamestate (perform-program program gamestate last-move))]                    ;30 moves used to calculate fitness but 50 when visualizing the moves          
            
                 (recur 
                   (move-player gamestate move)
                   (inc total-moves)
                   (if (>= total-moves 15)
                     (inc moves>15)                         ;only increments the moves>15 
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


(defn population-fitness                                                  ;mostly helpful for visualization of best functions
  "Returns a list of the fitness of each individual population"
  [population maze]
  (map (fn [x] (program-fitness x maze)) population))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;parent selection


(defn tournament-selection 
  "Randomly selects 3 programs from the population input and outputs the best one with the best fitness"
  [prog-pop maze]
  (loop [i 0
         lst '()]
    (if (not (>= i 3))
      (recur (inc i)
             (conj lst (rand-nth prog-pop)))
      (first (sort-by (fn [x] (program-fitness x maze)) lst)))))


(defn best-n-progs                          ;this helps us guide our evolution towards better programs and also sorts programs 
  "Returns the best n number of programs from the given program population"
  [prog-pop n maze]
  (take n (sort-by (fn [x] (program-fitness x maze)) prog-pop)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;mutations helpers


(def instructions
  '{andd 2                              
    orr 2
    iff 2})


(defn program-size
  "Finds the size of the program, i.e. number of nodes in its tree."
  [prog]
  (if (not (seq? prog))            ; if its not a sequence it will return 1.. this lets us evalute the size of a terminal
    1
    (count (flatten prog))))       ;flatten essentially removes all parenthesis so you can easily evaluate size of the tree


(defn select-random-subtree                                         
  "Given a program, selects a random subtree and returns it."         
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



(defn replace-random-subtree                                                                   
  "Given a program and a replacement-subtree, replace a random node       
   in the program with the replacement-subtree."
  ([prog replacement-subtree]
    (replace-random-subtree prog replacement-subtree (rand-int (program-size prog))))        
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mutation or variation


(defn replication                                  ; reproduction
  "Returns clone of the input program."
  [prog]
  prog)


(defn subtree-mutation
  "Selects a random node from an input program and replaces it with a random subtree
   of max-depth 3"
  [program]
  (replace-random-subtree program (grow 3)))    ;arbitrarily selected max-depth 3


(defn hoist-mutation                           ;our change from the basic GP system
  "Selects root-node from the input program and replaces it with a random subtree
   from the program"
  [program]
  (replace-random-subtree program (select-random-subtree program) 0))


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


(defn cross-over
  "Performs cross-over mutation on two input parents
   by replacing a random subtree from parent 1 with a
   random subtree in parent2"
  [parent1 parent2]
  (replace-random-subtree parent1 (select-random-subtree parent2)))

  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;main

(defn genetic-programming
  "This function only takes as an input which maze you want to test the genetic programming on.
   It generates an initial population size of 30, and produces
   new generations via different methods of mutation of population size 30.
   Each generation prints the generation number, the best program
   in that generation, the total error of that program, and the best 20 programs' total fitness.
   Will terminate at 50 generations or when a solution (fitness <15)is found."
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
        (<= best-err 15) (println "*****Solution Found!*****\n Solution is:" best-prog "\n") ;if the error is less than 15 that means it has solved the maze in atleast 30 moves. which is our criteria for termination
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




