;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Emampoor. Xing. PS9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
; Problem Set 9:
; Updated Frogger Game:

;Global Constants
(define WID 500)
(define HEI 250)
(define GRID (/ 500 26))
(define BG (rectangle WID (* 2 HEI) 'solid 'grey))

(define B_ROW (rectangle WID  (* 2 GRID) 'solid 'darkgreen))
(define M_ROW (rectangle WID (* 2 GRID) 'solid 'darkgreen))
(define T_ROW (rectangle WID  (* 2 GRID) 'solid 'darkgreen))


(define FROG-RADIUS 8)
(define SPEED (* 2 GRID))

(define EYE (overlay 
             (circle 2 'outline 'white)
             (circle 2 'solid 'black)))
(define FROG 
  (place-images
              (list EYE EYE)
              (list (make-posn 13 4)
                    (make-posn 3 4))
              (overlay
              (circle FROG-RADIUS 'outline 'black)
              (circle FROG-RADIUS 'solid 'green))))

(define FROGL (rotate 90 FROG))
(define FROGR (rotate 270 FROG))
(define FROGD (rotate 180 FROG))

(define RIVER (rectangle WID HEI 'solid 'cadetblue))

(define WHOLE_BG 
  (overlay/align
   "center"
   "bottom"
   B_ROW
   (overlay/align
    "center"
    "top"
    T_ROW
    (overlay/align
     "center"
     "center"
     M_ROW
     (overlay/align
      "center"
      "top"
      RIVER
      BG)))))

(define PLANK_WID (/ WID 5))
(define PLANK_HEI (/ HEI 15))
(define first-plank-pos (/ (+ WID PLANK_WID) 4))


(define T_WID (/ WID 15))
(define T_HEI (/ HEI 10))
(define first-turtle-pos (/ (+ WID T_WID) 5))

(define CAR_HEI (/ HEI 12))
(define CAR_WIDTH (/ WID 10))
(define car1 (rectangle CAR_WIDTH CAR_HEI 'solid 'darkslateblue))
(define first-x-pos (/ (+ WID CAR_WIDTH) 4))

(define PLANK (rectangle PLANK_WID PLANK_HEI 'solid 'brown))
(define TURTLE (underlay/offset
                (overlay
                 (circle 6 'outline 'black)
                 (circle 6 'solid 'darkolivegreen))
                16
                0
                (ellipse T_WID T_HEI 'solid 'darkolivegreen)))

;; Data Definitions

; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

;; A Player is a (make-player Number Number Direction)
(define-struct player (x y dir))
; interpretations:
; (make-player n1 n2 d) n1 is the x position of the player
;     and n2 is the y position of the player
;     and d is the direction of the player
(define frog1 (make-player 250 480 "up"))
(define frog2 (make-player 180 300 "left"))
; player-temp: Player -> ???
#;
(define (player-temp a-player)
  (...(player-x a-player)...
      (player-y a-player)...
      (player-dir a-player) ...))

;; A Vehicle is a (make-vehicle Number Number Direction)
(define-struct vehicle (x y dir))
; interpretations:
; (make-vehicle n3 n4 d1) n3 is the x position of the vehicle
;     and n4 is the y position of the vehicle
;     and d1 is the direction of the vehicle
; constraints:
; the direction of vehicle can only be "left" or "right"
(define v1 (make-vehicle 0 (* 23 GRID) "left"))
(define v2 (make-vehicle 0 (* 21 GRID) "right"))
(define v3 (make-vehicle 0 (* 19 GRID) "left"))
(define v4 (make-vehicle 0 (* 17 GRID) "right"))
(define v5 (make-vehicle 0 (* 15 GRID) "left"))
; vehicle-temp: Vehicle -> ???
#;
(define (vehicle-temp a-vehicle)
  (...(vehicle-x a-vehicle)...
      (vehicle-y a-vehicle)...
      (vehicle-dir a-vehicle)...))

;; A Set of Vehicles (VSet) is one of:
;; - empty
;; - (cons Vehicle VSet)
; The VSet represents all of the Vehicles on the scene

; vset-temp: VSet -> ???
#;(define (vset-temp a-vset)
    (cond
      [(empty? a-vset) ...]
      [(cons? a-vset) ... (first a-vset)... 
                      ... (vset-temp (rest a-vset))]))

; create-vset: Vehicle -> VSet
; Creates a list of vehicles
(define (create-vset vx)
  (local(; number->vehicle: Number -> Vehicle
         ; given a number, creates a vehicle
         (define (number->vehicle n)
           (make-vehicle (* n first-x-pos)
                         (vehicle-y vx)
                         (vehicle-dir vx))))
    (build-list 4 number->vehicle)))
(check-expect (create-vset v1)
              (list (make-vehicle 0
                                  (* 23 GRID)
                                  "left")
                    (make-vehicle 137.5
                                  (* 23 GRID)
                                  "left")
                    (make-vehicle 275
                                  (* 23 GRID)
                                  "left")
                    (make-vehicle 412.5
                                  (* 23 GRID)
                                  "left")))
(define VSet0 empty)
(define VSet1 (append (create-vset v1) 
                      (create-vset v2) 
                      (create-vset v3) 
                      (create-vset v4)
                      (create-vset v5)))
(define VSet2 (create-vset v1))

; A Plank is one of (make-plank Number Number)
(define-struct plank (x y))
; interpretations: (make-plank n1 n2)
; n1 is the x-position of the plank
; and n2 is the y-position of the plank
; plank-temp: Plank -> ???
#;
(define (plank-temp p)
  (... (plank-x p)...
       (plank-y p)...))
(define plank1 (make-plank 0 (* 3 GRID)))
(define plank2 (make-plank 0 (* 5 GRID)))
(define plank3 (make-plank 0 (* 9 GRID)))

; A LOP[List-of Plank] is one of:
; - empty
; - (cons Plank LOP)

;lop-temp: LOP -> ???
#; (define (lop-temp a-lop)
     (cond
       [(empty? a-lop) ...]
       [(cons? a-lop) ... (first a-lop)
                      ...(lop-temp (rest a-lop))]))

; create-lop: Plank -> LOP
; Creates a list of planks
(define (create-lop px)
  (local(; number->plank: Number -> Plank
         ; given a number, creates a plank
         (define (number->plank n)
           (make-plank (* n first-plank-pos)
                       (plank-y px))))
    (build-list 4 number->plank)))
(check-expect (create-lop plank1)
              (list (make-plank 0
                                (* 3 GRID))
                    (make-plank 150
                                (* 3 GRID))
                    (make-plank 300
                                (* 3 GRID))
                    (make-plank 450
                                (* 3 GRID))))
(define LOP1 (append (create-lop plank1)
                     (create-lop plank2)
                     (create-lop plank3)))
; A Turtle is one of (make-turtle Number Number)
(define-struct turtle (x y))
; interpretations: (make-turtle n1 n2)
; n1 is the x-position of the turtle
; and n2 is the y-position of the turtle
(define turtle1 (make-turtle 0 (* 7 GRID)))
(define turtle2 (make-turtle 0 (* 11 GRID)))
; turtle-temp: Turtle -> ???
#;
(define (turtle-temp t)
  (... (turtle-x t)...
       (turtle-y t)...))

; A LOT [List-of Turtle] is one of:
; - empty
; - (cons Turtle LOT)

; lot-temp: LOT -> ???
#; (define (lot-temp a-lot)
     (cond
       [(empty? a-lot) ...]
       [(cons? a-lot) ... (first a-lot) ... 
                      ... (lot-temp (rest a-lot))]))

; create-lot: Turtle -> LOT
; Creates a list of turtles
(define (create-lot tx)
  (local(; number->turtle: Number -> Turtle
         ; given a number, creates a turtle
         (define (number->turtle n)
           (make-turtle (* n first-turtle-pos)
                        (turtle-y tx))))
    (build-list 5 number->turtle)))
(check-expect (create-lot turtle1)
              (list (make-turtle 0
                                 (* 7 GRID))
                    (make-turtle first-turtle-pos
                                 (* 7 GRID))
                    (make-turtle (* 2 first-turtle-pos)
                                 (* 7 GRID))
                    (make-turtle (* 3 first-turtle-pos)
                                 (* 7 GRID))
                    (make-turtle (* 4 first-turtle-pos)
                                 (* 7 GRID))))
(define LOT1 (append (create-lot turtle1)
                     (create-lot turtle2)))

;; A World is a (make-world Player VSet LOT LOP)
(define-struct world (player vehicles turtles planks))
; interpretations: (make-world p vs alot alop)
; p is the player inside the world
; vs is the VSet inside the world
; alot is the [List-of Turtle] inside the world
; alop is the [List-of Plank] inside the world
(define world0 (make-world frog1 empty empty empty))
(define world1 (make-world frog1 VSet1 LOT1 LOP1))
(define world2 (make-world frog1 
                           (list
                            (make-vehicle 0 160 "left")
                            (make-vehicle 125 160 "left"))
                           (list
                            (make-turtle 0 0))
                           (list
                            (make-plank 0 0))))
(define world3 (make-world frog1 (list
                                  (make-vehicle 0 220 "right")
                                  (make-vehicle 125 220 "right"))  
                           empty
                           empty))

; world-temp: World -> ???
#;
(define (world-temp a-world)
  (... (world-player a-world)...
       (world-vehicles a-world)...
       (world-turtles a-world)...
       (world-planks a-world)...))


;;;;;;;;; GAME  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; main: World -> World
; launches the Frogger game
(define (main x)
  (big-bang world1
            (to-draw draw-world)
            (on-tick move-those)
            (on-key move-frog)
            (stop-when game-over?)))


; draw-world: World -> Image
; places the player, vehicles, planks, and turtles onto the background
(define (draw-world w)
  (place-image (facing (world-player w))
               (player-x (world-player w))
               (player-y (world-player w))
               (draw-planks w)))
(check-expect (draw-world world2)
              (place-image
               FROG
               250
               480
               (place-image
                PLANK
                0
                0
                (place-image
                 TURTLE
                 0 
                 0
                 (place-images 
                  (list car1 car1)
                  (list (make-posn 0 160)
                        (make-posn 125 160))
                  WHOLE_BG)))))

; facing: Player -> Image
; chooses the image of the frog based on which direction it is going
(define (facing frog)
  (cond
    [(string=? (player-dir frog) "right")
     FROGR]
    [(string=? (player-dir frog) "left")
     FROGL]
    [(string=? (player-dir frog) "down")
     FROGD]
    [(string=? (player-dir frog) "up")
     FROG]))
(check-expect (facing (make-player 240 300 "left")) FROGL)
(check-expect (facing (make-player 240 300 "right")) FROGR)
(check-expect (facing (make-player 240 300 "down")) FROGD)
(check-expect (facing (make-player 240 300 "up")) FROG)

; draw-cars: World -> Image
; adds the vehicles to the background
(define (draw-cars w)
  (local [; place: Posn Image -> Image
          ; adds image on the bg based on the posn
          (define (place p i)
            (place-image
             car1
             (posn-x p)
             (posn-y p)
             i))]
    (foldr place WHOLE_BG (veh-posn w))))
(check-expect (draw-cars world3)
              (place-images
               (list car1 car1)
               (list (make-posn 0 220) (make-posn 125 220))
               WHOLE_BG))

; veh-posn: World -> [List-of Posn]
; creates a list of the vehicle positions
(define (veh-posn w)
  (cond
    [(empty? (world-vehicles w)) empty]
    [(cons? (world-vehicles w)) 
     (cons (make-posn (vehicle-x (first (world-vehicles w))) 
                      (vehicle-y (first (world-vehicles w)))) 
           (veh-posn (make-world (world-player w) 
                                 (rest (world-vehicles w)) 
                                 (world-turtles w) 
                                 (world-planks w))))]))

(check-expect (veh-posn world0) empty)
(check-expect (veh-posn world3) (list (make-posn 0 220)
                                      (make-posn 125 220)))

; draw-turtles: World -> Image
; adds the turtles to the image with the cars
(define (draw-turtles w)
  (local [; place: Posn Image -> Image
          ; adds image on the bg based on the posn
          (define (place p i)
            (place-image
             TURTLE
             (turtle-x p)
             (turtle-y p)
             i))]
    (foldr place (draw-cars w) (world-turtles w))))  

(check-expect (draw-turtles world2)
              (place-image
               TURTLE
               0 
               0
               (place-images 
                (list car1 car1)
                (list (make-posn 0 160)
                      (make-posn 125 160))
                WHOLE_BG)))

; draw-planks: World -> Image
; adds the planks to the image with the turtles and vehicles
(define (draw-planks w)
  (local [; Posn Image -> Image
          ; adds image on the bg based on the posn
          (define (place p i)
            (place-image
             PLANK
             (plank-x p)
             (plank-y p)
             i))]
    (foldr place (draw-turtles w) (world-planks w))))

(check-expect (draw-planks world2)
              (place-image
               PLANK
               0
               0
               (place-image
                TURTLE
                0 
                0
                (place-images 
                 (list car1 car1)
                 (list (make-posn 0 160)
                       (make-posn 125 160))
                 WHOLE_BG))))

;;;;;;;;;;;;;;;;on tick;;;;;;;;;;;;;;;;;

; move-those: World -> World
; updates the game by moving vehicles and planks and turtles at the next tick
(define (move-those w)
  (make-world
   (sit-on-move w)
   (go-vehicle (world-vehicles w))
   (go-turtle (world-turtles w))
   (go-plank (world-planks w))))

(check-expect (move-those world2)
              (make-world frog1 (list
                                 (make-vehicle -2 160 "left")
                                 (make-vehicle 123 160 "left"))
                          (list
                           (make-turtle -1 0))
                          (list
                           (make-plank 2 0))))

; go-vehicle: VSet -> VSet
; makes a list of vehicles which moved on tick
(define (go-vehicle vs)
  (map check-move vs))

(check-expect (go-vehicle (list (make-vehicle 300 400 "left") 
                                (make-vehicle 300 400 "right")
                                (make-vehicle -25 300 "left")
                                (make-vehicle 525 300 "right")))
              (list (make-vehicle 298 400 "left")
                    (make-vehicle 302 400 "right")
                    (make-vehicle 525 300 "left")
                    (make-vehicle -25 300 "right")))

; check-move: Vehicle -> Vehicle
; check the direction of the vechicle and
; moves the vehicles at the next tick
; if the vehicle has hit the edge, the function creates a
; new vehicle on the opposite side of the screen
(define (check-move veh)
  (local(; check: String [Number Number -> Boolean] Number -> Boolean
         ; Checks whether the vehicle has hit the edge
         (define (check dir comp edge)
           (and (string=? dir (vehicle-dir veh))
                (comp (vehicle-x veh) edge)))
         ; create-veh: Number -> Vehicle
         ; creates a vehicle with the new x-position
         (define (create-veh change)
           (make-vehicle change
                         (vehicle-y veh)
                         (vehicle-dir veh))))
    (cond
      [(check "left" <= (* -1 (/ CAR_WIDTH 2)))
       (create-veh (+ WID (/ CAR_WIDTH 2)))]
      [(check "left" > (* -1 (/ CAR_WIDTH 2)))
       (create-veh (- (vehicle-x veh) 2))]
      [(check "right" >= (+ WID (/ CAR_WIDTH 2)))
       (create-veh (* -1 (/ CAR_WIDTH 2)))]
      [(check "right" < (+ WID (/ CAR_WIDTH 2)))
       (create-veh (+ (vehicle-x veh) 2))])))

(check-expect (check-move (make-vehicle 300 400 "left"))
              (make-vehicle 298 400 "left"))
(check-expect (check-move (make-vehicle 300 400 "right"))
              (make-vehicle 302 400 "right"))
(check-expect (check-move (make-vehicle -25 300 "left"))
              (make-vehicle 525 300 "left"))
(check-expect (check-move (make-vehicle 525 300 "right"))
              (make-vehicle -25 300 "right"))

; go-turtle: LOT -> LOT
; makes a list of moved turtles
(define (go-turtle alot)
  (map check-move-t alot))
(check-expect (go-turtle (list (make-turtle -25 400)
                               (make-turtle 40 400)))
              (list (make-turtle (+ 500 (/ T_WID 2)) 400)
                    (make-turtle 39 400)))

; check-move-t: Turtle -> Turtle
; moves the turtle at the next tick
; if the turtle has hit the edge, the function creates a
; new turtle on the opposite side of the screen
(define (check-move-t t)
  (local [; check: [Number Number -> Boolean] Number -> Boolean
          ; Checks whether the turtle has hit the edge
          (define (check comp edge)
            (comp (turtle-x t) edge))
          ; create-t: Number -> Turtle
          ; creates a turtle with the new x-position
          (define (create-t change)
            (make-turtle change
                         (turtle-y t)))]
    (cond
      [(check <= (* -1 (/ T_WID 2)))
       (create-t (+ WID (/ T_WID 2)))]
      [(check > (* -1 (/ T_WID 2)))
       (create-t (- (turtle-x t) 1))])))

(check-expect (check-move-t (make-turtle (* -1 (/ T_WID 2)) 400))
              (make-turtle (+ 500 (/ T_WID 2)) 400))
(check-expect (check-move-t (make-turtle 40 400))
              (make-turtle 39 400))

; go-plank: LOP -> LOP
; makes a list of planks which moved on tick
(define (go-plank alop)
  (map check-move-p alop))

(check-expect (go-plank (list (make-plank 550 440)
                              (make-plank 300 500)))
              (list (make-plank -50 440)
                    (make-plank 302 500)))

; check-move-p: Plank -> Plank
; moves the plank at the next tick
; if the plank has hit the edge, the function creates a
; new plank on the opposite side of the screen
(define (check-move-p p)
  (local [; check: [Number Number -> Boolean] Number -> Boolean
          ; Checks whether the plank has hit the edge
          (define (check comp edge)
            (comp (plank-x p) edge))
          ; create-p: Number -> Plank
          ; creates a plank with the new x-position
          (define (create-p change)
            (make-plank change
                        (plank-y p)))]
    (cond
      [(check >= (+ WID (/ PLANK_WID 2)))
       (create-p (* -1 (/ PLANK_WID 2)))]
      [(check < (+ WID (/ PLANK_WID 2)))
       (create-p (+ (plank-x p) 2))])))

(check-expect (check-move-p (make-plank 550 440))
              (make-plank -50 440))
(check-expect (check-move-p (make-plank 300 500))
              (make-plank 302 500))

;;;;;;;;;;;;;;;;;;;;;;;;;;on-key ;;;;;;;;;;;;;;;;;;;;;;;;;

; move-frog: World KeyEvent -> World
; creates a new world with a frog from go-frog
(define (move-frog w ke)
  (make-world (go-frog (world-player w) ke)
              (world-vehicles w) 
              (world-turtles w) 
              (world-planks w)))
(check-expect (move-frog world0 "up")
              (make-world (make-player 250 (- 480 (* 2 GRID)) "up")
                          empty
                          empty
                          empty))
(check-expect (move-frog world2 "up")
              (make-world (make-player 250 (- 480 (* 2 GRID)) "up")
                          (list
                           (make-vehicle 0 160 "left")
                           (make-vehicle 125 160 "left"))
                          (list
                           (make-turtle 0 0))
                          (list
                           (make-plank 0 0))))

; go-frog: Player KeyEvent -> Player
; moves the frog on the screen
(define (go-frog f ke)
  (local(; checkf: Direction [Number Number -> Boolean] Number Number -> Boolean
         ; Checks if the frog has hit the edge
         (define (checkf dir comp pos edge)
           (and (string=? ke dir)
                (comp pos edge)))
         ; move-pla: Operator String -> Player
         ; moves the player based on the given KeyEvent
         (define (move-pla op p)
           (cond
             [(string=? p "x") 
              (make-player (op (player-x f) SPEED) 
                           (player-y f) 
                           ke)]
             [(string=? p "y")
              (make-player (player-x f) 
                           (op (player-y f) SPEED) 
                           ke)])))
    (cond
      [(checkf "up" >= (player-y f) (* 2 FROG-RADIUS))
       (move-pla - "y")]
      [(checkf "down" <= (player-y f) (- (* 2 HEI) (* 2 FROG-RADIUS)))
       (move-pla + "y")]
      [(checkf "left" >= (player-x f) (* 2 FROG-RADIUS))
       (move-pla - "x")]
      [(checkf "right" <= (player-x f) (- WID (* 2 FROG-RADIUS)))
       (move-pla + "x")]
      [else f])))

(check-expect (go-frog frog1 "up")
              (make-player 250 (- 480 (* 2 GRID)) "up"))
(check-expect (go-frog (make-player 250 0 "up") "up")
              (make-player 250 0 "up"))
(check-expect (go-frog (make-player 250 500 "down") "down")
              (make-player 250 500 "down"))
(check-expect (go-frog (make-player 250 300 "down") "down")
              (make-player 250 (+ 300 (* 2 GRID)) "down"))
(check-expect (go-frog (make-player 0 200 "left") "left")
              (make-player 0 200 "left"))
(check-expect (go-frog (make-player 300 300 "left") "left")
              (make-player (- 300 (* 2 GRID)) 300 "left"))
(check-expect (go-frog (make-player 500 300 "right") "right")
              (make-player 500 300 "right"))
(check-expect (go-frog (make-player 300 300 "right") "right")
              (make-player (+ 300 (* 2 GRID)) 300 "right"))
(check-expect (go-frog (make-player 300 300 "right") "f")
              (make-player 300 300 "right"))

;sit-on-move: World -> Player
; Moves the player with a plank or turtle if the player is sitting on it
(define (sit-on-move w)
  (cond
    [(ormap (sitting-turtle? (world-player w)) (world-turtles w)) 
     (make-player (- (player-x (world-player w)) 1)
                  (player-y (world-player w))
                  (player-dir (world-player w)))]
    [(ormap (sitting-plank? (world-player w)) (world-planks w))
     (make-player (+ (player-x (world-player w)) 2)
                  (player-y (world-player w))
                  (player-dir (world-player w)))]
    [else (world-player w)]))

(check-expect (sit-on-move world2) frog1)
(check-expect (sit-on-move (make-world (make-player 100 100 "left") 
                                       empty
                                       (list (make-turtle 100 105))
                                       (list (make-plank 200 100))))
              (make-player 99 100 "left"))
(check-expect (sit-on-move (make-world (make-player 150 170 "up")
                                       empty
                                       empty
                                       (list (make-plank 150 172))))
              (make-player 152 170 "up"))

;;;;;;;;;;;;;;;;;;;;; stop-when ;;;;;;;;;;;;;;;;;;

; game-over?: World -> Boolean
; checks if any of the conditions is true and
; ends the game if any of the condition is true
(define (game-over? w)
  (or (hit-cars? (world-vehicles w) (world-player w))
      (drowned? w)
      (hit-edge? (world-player w))
      (win? (world-player w))))

(check-expect (game-over? world1) false)
(check-expect (game-over? world0) false)
(check-expect (game-over? (make-world 
                           (make-player 250 10 "up") 
                           VSet1
                           LOT1
                           LOP1))
              true)
(check-expect (game-over? (make-world (make-player 100 100 "up")
                                      empty
                                      empty
                                      empty))
              true)
(check-expect (game-over? (make-world (make-player 400 400 "down")
                                      (list (make-vehicle 400 400 "left"))
                                      empty
                                      empty))
              true)
(check-expect (game-over? (make-world (make-player 5 0 "right")
                                      empty
                                      empty
                                      empty))
              true)
; hit-cars?: VSet Player -> Boolean
; checks whether the frog collides with any of the cars or not
; if it collides with any car, then ends the game
(define (hit-cars? vs p)
  (ormap (λ (v) (check-collision v p)) vs))

(check-expect (hit-cars? empty (make-player 20 30 "up")) false)
(check-expect (hit-cars? (list (make-vehicle 0 220 "left")
                               (make-vehicle 125 220 "right"))
                         (make-player 250 460 "up"))
              false)
(check-expect (hit-cars? (list (make-vehicle 0 220 "left")
                               (make-vehicle 250 440 "right"))
                         (make-player 250 450 "up"))
              true)

; check-collision: Player -> Boolean
; checks if the frog collides with a car
; if it collides with a car, then ends the game
(define (check-collision v1 p1)
  (and (<= (- (player-x p1)
              (vehicle-x v1))
           (+ FROG-RADIUS (/ CAR_WIDTH 2)))
       (<= (- (vehicle-x v1)
              (player-x p1))
           (+ FROG-RADIUS (/ CAR_WIDTH 2)))
       (<= (- (player-y p1)
              (vehicle-y v1))
           (+ FROG-RADIUS (/ CAR_HEI 2)))
       (<= (- (vehicle-y v1)
              (player-y p1))
           (+ FROG-RADIUS (/ CAR_HEI 2)))))

(check-expect (check-collision (make-vehicle 180 288 "left")
                               (make-player 213 288 "up"))
              true)
(check-expect (check-collision (make-vehicle 46 300 "left")
                               (make-player 400 400 "up"))
              false)
(check-expect (check-collision (make-vehicle 300 300 "right")
                               (make-player 267 300 "left"))
              true)
(check-expect (check-collision (make-vehicle 300 300 "right")
                               (make-player 200 200 "right"))
              false)
(check-expect (check-collision (make-vehicle 300 300 "right")
                               (make-player 300 295 "right"))
              true)
(check-expect (check-collision (make-vehicle 300 300 "right")
                               (make-player 100 150 "down"))
              false)
(check-expect (check-collision (make-vehicle 300 195 "right")
                               (make-player 300 200 "right"))
              true)
(check-expect (check-collision (make-vehicle 300 156 "right")
                               (make-player 200 200 "right"))
              false)

; hit-edge?: Player -> Boolean
; checks whether the player hits the edge of the scene
; if so, game is over
(define (hit-edge? p)
  (or (<= (player-y p) (* 2 FROG-RADIUS))
      (>= (player-y p) (* 2 HEI))
      (<= (player-x p) (* 2 FROG-RADIUS))
      (>= (player-x p) (- WID (* 2 FROG-RADIUS)))))
(check-expect (hit-edge? frog1) false)
(check-expect (hit-edge? (make-player 500 250 "up")) true)
(check-expect (hit-edge? (make-player 0 0 "up")) true)
(check-expect (hit-edge? (make-player 0 500 "up")) true)

; drowned?: World -> Boolean
; checks if the frog has drowned
(define (drowned? w)
  (and (< (player-y (world-player w)) (- HEI GRID))
       (not (ormap (sitting-plank? (world-player w)) (world-planks w)))
       (not (ormap (sitting-turtle? (world-player w)) (world-turtles w)))))

(check-expect (drowned? world0) false)
(check-expect (drowned? (make-world (make-player 200 200 "left")
                                    (list (make-vehicle 0 0 "right")) 
                                    (list (make-turtle 100 200))
                                    (list (make-plank 100 150)))) 
              true)
(check-expect (drowned? (make-world (make-player 100 200 "left")
                                    (list (make-vehicle 0 0 "right")) 
                                    (list (make-turtle 100 200))
                                    (list (make-plank 100 150))))
              false)

; sitting-plank?: Player -> [Plank -> Boolean]
; checks if the frog is sitting on a plank
(define (sitting-plank? frog)
  (λ (p1) (and (<= (player-x frog) (+ (plank-x p1) (/ PLANK_WID 2)))
               (>= (player-x frog) (- (plank-x p1) (/ PLANK_WID 2)))
               (<= (player-y frog) (+ (plank-y p1) (/ PLANK_HEI 2)))
               (>= (player-y frog) (- (plank-y p1) (/ PLANK_HEI 2))))))

(check-expect ((sitting-plank? (make-player 430 340 "right"))
               (make-plank 430 335)) true)
(check-expect ((sitting-plank? (make-player 430 340 "left"))
               (make-plank 430 300)) false)
(check-expect ((sitting-plank? (make-player 200 200 "right"))
               (make-plank 400 400)) false)
(check-expect ((sitting-plank? (make-player 410 410 "right"))
               (make-plank 405 405)) true)

; sitting-turtle?: Player -> [Turtle -> Boolean]
; Checks if the frog is sitting on a turtle
(define (sitting-turtle?  frog)
  (λ (t1) (and (<= (player-x frog) (+ (turtle-x t1) (/ T_WID 2)))
               (>= (player-x frog) (- (turtle-x t1) (/ T_WID 2)))
               (<=  (player-y frog) (+ (turtle-y t1) (/ T_HEI 2)))
               (>= (player-y frog) (- (turtle-y t1) (/ T_HEI 2))))))

(check-expect ((sitting-turtle? (make-player 300 300 "right"))
               (make-turtle 310 310)) true)
(check-expect ((sitting-turtle? (make-player 300 300 "left"))
               (make-turtle 300 100)) false)
(check-expect ((sitting-turtle? (make-player 300 300 "left"))
               (make-turtle 500 400)) false)
(check-expect ((sitting-turtle? (make-player 150 160 "right"))
               (make-turtle 400 160)) false)

; win?: Player -> Boolean
; checks if the player has made it to the top of the scene
(define (win? p)
  (<= (player-y p)
      (- (/ HEI 13) FROG-RADIUS)))
(check-expect (win? (make-player 50 80 "up"))
              false)
(check-expect (win? (make-player 250 10 "up"))
              true)

(main 1)