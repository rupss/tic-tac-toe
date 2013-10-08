(ns tic-tac-toe.core
  (:gen-class))

(def board-symbol-map
	{ nil "__" :x "X" :o "O" })

(defn nth-replace
	[original place replacement]
	(cond (empty? original) nil
		(= place 0)
		(cons replacement (rest original))
		true (cons (first original)
			(nth-replace (rest original) (- place 1) replacement))))

(defn get-empty-board
	[]
	  (into [] (repeat 3
	  	(into [] (repeat 3 nil)))))

(defn print-board
	"Prints board to screen. Right now, is very minimalistic and not super pretty."
	[board]
	(dotimes [n (count board)]
		(println 
			(apply str 
				(interpose " " (map (fn [x] (board-symbol-map x)) (nth board n)))))))

(defn num-is-valid
	"Returns true if 0 <= num < 3, false if not."
	[num]
	(if (and (>= num 0) (< num 3))
			true
			false))

(defn parse-letter
	"letter is a string"
	[letter]
	(let [num (- (int (nth (.toLowerCase letter) 0)) (int \a))]
		(if (num-is-valid num)
			num
			nil)))

(defn parse-int
	"number is a string - should be a string of an int"
	[number]
	(try 
		(let [num (. Integer parseInt number)]
			(if (num-is-valid num)
				num
				nil))
		(catch Exception e 
			nil)))

(defn get-square
	"Move will have the format <letter><number> e.g A1
	Refers to <row><column>"
	[move]
	(let [row (parse-letter (str (nth move 0)))
		  col (parse-int (str (nth move 1)))
		  ]
		(if (or (nil? row) (nil? col))
			nil
			(list row col))))

(defn get-modified-row
	[board row col curr-player]
	(let [working-row (nth board row)]
		(if (not (nth working-row col))
			(into [] (nth-replace working-row col curr-player))
			working-row)))

(defn update-board
	[board row col curr-player]
	(into [] (nth-replace board row (get-modified-row board row col curr-player))))

(defn apply-move
	[board curr-player]
	(let [square (get-square (read-line))]
			(if (nil? square)
				(println "Invalid move")
				(update-board board (first square) (last square) curr-player))))

(defn check-horizontal-winner
	[board player]
	(some (fn[row] (every? (fn[col] (= col player)) row)) board))

(defn check-vertical-winner
	[board player]
	)
(defn get-diagonal-vectors
	[board]
	(vector (vector (get-move board 0 0)
					(get-move board 1 1)
					(get-move board 2 2)
			(vector (get-move board 0 2)
					(get-move board 1 1)
					(get-move board 2 0)))))

(defn check-diagonal-winner
	[board player]
	(let [result [[]]]
		(conj result 1)
		(println result)))

(defn get-move
	[board row col]
	(nth (nth board row) col))

(defn -main
  "Tic-tac-toe main method"
  [& args]
  (println "Hello, World!")
  (print-board (get-empty-board))
  (print-board (apply-move (get-empty-board) :o)))
 ; (print (parse-letter "a")))



