(def PI math/pi)

# Color palette https://lospec.com/palette-list/pico-8
(def BLACK 0x000000FF)
(def NAVY 0x1D2B53FF)
(def MAROON 0x7E2553FF)
(def GREEN 0x008751FF)
(def BROWN 0xAB5236FF)
(def CHARCOAL 0x5F574FFF)
(def GRAY 0xC2C3C7FF)
(def WHITE 0xFFF1E8FF)
(def RED 0xFF004DFF)
(def ORANGE 0xFFA300FF)
(def YELLOW 0xFFEC27FF)
(def LIME 0x00E436FF)
(def BLUE 0x29ADFFFF)
(def HEATHER 0x83769CF)
(def PINK 0xFF77A8FF)
(def SKIN 0xFFCCAAFF)

(def W 800)
(def H 450)
(def +player-max-life+ 5)
(def +lines-of-bricks+ 5)
(def +bricks-per-line+ 10)

(var FRAME 0)
(var GAMEOVER? false)
(var PAUSED? false)

(def ENTITIES @[])
(def PARTICLES @[])

(def PLAYER @{})
(def BALL @{})

(def DEV? true)

(use jaylib)
(import spork/netrepl)

# ECS

(defn- has-all-keys? [ds keys]
  (all |(has-key? ds $) keys))

(defn system/do [entities components function]
  (loop [e :in (filter |(has-all-keys? $ components) entities)]
    (function e)))

(defn system/any [entities components pred]
  (some pred (filter |(has-all-keys? $ components) entities)))

(defn system/all [entities components pred]
  (all pred (filter |(has-all-keys? $ components) entities)))

(defn system/draw [&opt entities]
  (default entities ENTITIES)
  (system/do entities [:draw] :draw))

(defn system/update [&opt entities]
  (default entities ENTITIES)
  (system/do entities [:update] :update))

(defn system/collide [ball &opt entities]
  (default entities ENTITIES)
  (system/do entities [:break]
             (fn [e]
               (when (and (not (e :hidden))
                          (check-collision-circle-rec (ball :pos) (ball :r)
                                                      [;(e :pos) ;(e :size)]))
                 (:break e)))))

(defn system/gc [&opt entities]
  (default entities ENTITIES)
  (loop [i :down-to [(dec (length entities)) 0]
         :let [e (get entities i)]]
    (when (e :remove)
      (array/remove entities i))))

# Particle system

(defn particle/update [self]
  (var final-update? false)
  (let [{:friction friction :r r :vel [vx vy] :tick tick :colors colors} self]
    (update-in self [:pos 0] + vx)
    (update-in self [:pos 1] + vy)
    (update-in self [:vel 0] * friction)
    (update-in self [:vel 1] * friction)
    (when (< tick 5)
      (/= (self :r) 1.1)
      (when (and (zero? (% FRAME 10)) (< 1 (length colors)))
        (array/pop (self :colors))))
    (update self :tick + -1 (math/random)))

  (when (< (self :r) 1)
    (set (self :remove) true)))

(defn particle/draw [{:pos pos :r r :colors colors}]
  (draw-circle-v pos r (array/peek colors)))

(defn particle/init [pos]
  (def angle (+ (* (math/random) PI) PI))
  @{:pos @[;pos]
    :r (* (math/random) 3)
    :friction 0.92
    :tick 20
    :vel @[(math/cos angle) (math/sin angle)]
    :colors @[SKIN PINK HEATHER BLUE] # @[0x333C57FF 0x566C86FF 0x94B0C2FF 0xF4F4F4FF]
    :update particle/update
    :draw particle/draw})

(defn particle-system/dust-clout-at [point &opt count]
  (for i 0 (default count 8)
    (array/push PARTICLES (particle/init point))))

(defn player/draw [{:pos [x y] :size [w h] :life life}]
  (draw-rectangle x y w h LIME)
  (for i 0 life
    (draw-rectangle (+ 20 (* 40 i)) (- H 30) 35 10 CHARCOAL)))

(defn player/update [self]
  (when (key-down? :left)
    (update-in self [:pos 0] - 5))
  (when (neg? (get-in self [:pos 0]))
    (put-in self [:pos 0] 0))
  (when (key-down? :right)
    (update-in self [:pos 0] + 5))
  (when (<= W (+ (get-in self [:pos 0]) (get-in self [:size 0])))
    (put-in self [:pos 0] (- W (get-in self [:size 0])))))

(defn player/init []
  {:pos @[(- (div W 2) (div W 20)) (div (* H 7) 8)]
   :size @[(div W 10) 20]
   :life +player-max-life+
   :draw player/draw
   :update player/update})

(defn ball/draw [{:pos pos :r r}]
  (draw-circle-v pos r BLUE))

(defn ball/update [self]
  # ball launching
  (when (not (self :active))
    (when (key-pressed? :space)
      (set (self :active) true)
      (put-in self [:v 0] 0)
      (put-in self [:v 1] -5)))

  # ball movement
  (if (self :active)
    (do
      (update-in self [:pos 0] + (get-in self [:v 0]))
      (update-in self [:pos 1] + (get-in self [:v 1])))
    (do
      (put-in self [:pos 0] (+ (get-in PLAYER [:pos 0]) (div (get-in PLAYER [:size 0]) 2)))
      (put-in self [:pos 1] (- (div (* H 7) 8) 30))))

  # collision: ball vs walls
  (let [{:pos [x y] :r r :v [vx vy]} self]
    (when (or (<= W (+ x r))
              (<= (- x r) 0))
      (update-in self [:v 0] * -1))
    (when (<= (- y r) 0)
      (update-in self [:v 1] * -1))
    (when (<= H (+ y r))
      (put-in self [:v 0] 0)
      (put-in self [:v 1] 0)
      (set (self :active) false)
      (update PLAYER :life + -1)))

  # collision: ball vs player
  (let [{:pos [x y] :size [w h]} PLAYER
        {:pos [bx by] :r br :v [bvx bvy]} self]
    (when (check-collision-circle-rec [bx by] br [x y w h])
      (particle-system/dust-clout-at [bx by])
      (when (pos? bvy)
        (update-in self [:v 1] * -1)
        (put-in self [:v 0] (div (* (- bx x (div w 2)) 5) (div w 2))))))

  # collision: ball vs bricks
  (system/collide BALL)
  )

(defn ball/init []
  {:pos @[(div W 2) (- (div (* H 7) 8) 30)]
   :v @[0 0]
   :r 7
   :active false
   :draw ball/draw
   :update ball/update})

(defn brick/draw [{:pos [x y] :size [w h] :color color :hidden hidden}]
  (when (not hidden)
    (draw-rectangle x y w h color)))

(defn brick/break [self]
  (set (self :hidden) true))

(defn- noop [& args] nil)

(defn game/init []
  (array/clear ENTITIES)

  (merge-into PLAYER (player/init))
  (array/push ENTITIES PLAYER)

  (merge-into BALL (ball/init))
  (array/push ENTITIES BALL)

  (def brick-w (div (get-screen-width) +bricks-per-line+))
  (def brick-h 40)
  (def initial-down-position 50)
  (for i 0 +lines-of-bricks+
    (for j 0 +bricks-per-line+
      (def brick @{:pos [(* j brick-w) (+ (* i brick-h) initial-down-position)]
                   :size [brick-w brick-h]
                   :color (if (zero? (% (+ j i) 2))
                            YELLOW
                            ORANGE)
                   :break brick/break
                   :hidden false
                   :draw brick/draw
                   :update noop})
      (array/push ENTITIES brick))))

(defn game/over? []
  (cond
    (<= (PLAYER :life) 0) (set GAMEOVER? true)
    (system/all ENTITIES [:break] |($ :hidden)) (set GAMEOVER? true)
    nil))

(defn game/update []
  (when GAMEOVER?
    (when (key-pressed? :enter)
      (game/init)
      (set GAMEOVER? false)))

  (when (not GAMEOVER?)
    (when (key-pressed? :p)
      (set PAUSED? (not PAUSED?)))

    (when (not PAUSED?)
      (system/update)
      (system/update PARTICLES)
      (system/gc PARTICLES)
      (game/over?))))

(defn game/draw []
  (begin-drawing)

  (clear-background BLACK)

  (when (not GAMEOVER?)
    (system/draw PARTICLES)
    (system/draw)

    (when PAUSED?
      (draw-text "GAME PAUSED"
                 (- (div W 2)
                    (div (measure-text "GAME PAUSED" 40) 2))
                 (- (div H 2) 40)
                 40 ORANGE)))

  (when GAMEOVER?
    (draw-text "PRESS [ENTER] TO PLAY AGIAN"
               (- (div W 2)
                  (div (measure-text "PRESS [ENTER] TO PLAY AGIAN" 20) 2))
               (- (div H 2) 50)
               20 YELLOW))

  (end-drawing))

(defn start []
  (init-window W H "game")
  (game/init)
  (set-target-fps 60)

  (while (not (window-should-close))
    (ev/sleep 0)
    (game/update)
    (game/draw)
    (++ FRAME))

  (close-window))

# (def game (ev/call start))
# (start)

(defn main [&]
  (if DEV?
    (netrepl/server-single "127.0.0.1" "9365" (fiber/getenv (fiber/current)))
    (start)))

(def dialog
  ``
~ Character Name
| Each 'bar' represents a new panel to display. Newlines are optional but help when writing.
| (voice 2) The voice number is the sfx id to play on every character.
| (color 4) The number 4 is red in tic80 (color 12), and 12 is white.
| (speed 0.1) Speed will change the time between character progression, (speed 1) and can be any positive number.
| (wiggle true) wiggle will make the text bounce up and down, how fun!
``
  )


(def script-grammar
  ~{:bool (+ (* "true" (constant true))
             (* "false" (constant false)))
    :pos-int (number (some :d))
    :pos-float (number (some (+ :d ".")))
    :command (replace
               (* "("
                  (+ (* "voice " (constant :voice) :pos-int)
                     (* "speed " (constant :speed) :pos-float)
                     (* "color " (constant :color) :pos-int)
                     (* "wiggle " (constant :wiggle) :bool))
                  ")" (any " "))
               ,struct)
    :word (replace
            (* (constant :word)
               (<- (* (some (+ :w (set "',.?!"))) (any " "))))
            ,struct)
    :character-name (replace
                      (* (constant :name)
                         (replace (* (<- (some (+ :w " "))))
                                  ,string/trim) )
                      ,struct)
    :source-line (* "~ " :character-name (any :command))
    :dialogue-line (* "| " (some (choice :command :word)) (constant :wait))
    :main (* :source-line (some :dialogue-line))})

(peg/match script-grammar "~ Character Name
| (color 1) hello (wiggle true) world!
| (speed 0.5) pizza hotdog.")
