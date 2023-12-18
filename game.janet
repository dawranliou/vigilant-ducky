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

(def W 240)
(def H 136)
(def ZOOM 4)
(def SCREEN_W (* W ZOOM))
(def SCREEN_H (* H ZOOM))
(def PLAYER_MAX_LIFE 5)
(def LINES_OF_BRICKS 5)
(def BRICKS_PER_LINE 10)
(def BRICK_W 20)
(def BRICK_H 5)
(def BRICK_GAP (/ (- W (* BRICK_W BRICKS_PER_LINE)) (+ 1 BRICKS_PER_LINE)))
(def TITLE_FONT_SIZE 16)
(def FONT_SIZE 8)

(var FRAME 0)
(var GAMEOVER? false)
(var PAUSED? false)

(def ENTITIES @[])
(def PARTICLES @[])

(def PLAYER @{})
(def BALL @{})

(def DEV? (truthy? (dyn 'DEV)))
(def RNG (math/rng (os/time)))

(use jaylib)

(var CAMERA nil)
(def SHAKER @{:amplitude 0 :duration 0})

(defn- noop [& args] nil)

(defn out [f]
  (fn [s & args]
    (- 1 (f (- 1 s) ;args))))

(defn chain [f1 f2]
  (fn [s & args]
    (if (< s 0.5)
      (f1 (* 2 s) ;args)
      (+ 1 (f2 (- (* 2 s) 1) ;args)))))

(defn linear [s] s)
(defn quad [s] (* s s))
(defn cubic [s] (* s s s))
(defn elastic [s]
  (let [amp 1 period 0.3]
    (* -1 amp
       (math/sin (- (/ (* 2 math/pi (- s 1)) period)
                    (math/asin (/ 1 amp))))
       (math/exp2 (* 10 (- s 1))))))

(defn lerp [t a b &opt func]
  (default func linear)
  (+ a (* (- b a) (func t))))


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

(defn system/update [dt &opt entities]
  (default entities ENTITIES)
  (system/do entities [:update] |(:update $ dt)))

(defn system/collide [ball &opt entities]
  (default entities ENTITIES)
  (var collided-x? false)
  (var collided-y? false)
  (system/do entities [:break]
             (fn [e]
               (when (and (not (e :hidden))
                          (check-collision-circle-rec (ball :pos) (ball :radius)
                                                      [;(e :pos) ;(e :size)]))
                 # (let [{:pos [x y] :radius r} ball
                 #       {:pos [ex ey] :size [ew eh]} e]
                 #   # Collide horizonally
                 #   (when (or (< ex (+ x r) (+ ex ew))
                 #             (< ex (- x r) (+ ex ew)))
                 #     (set collided-y? true))
                 #   # Collide vertically
                 #   (when (or (< ey (+ y r) (+ ey eh))
                 #             (< ey (- y r) (+ ey eh)))
                 #     (set collided-x? true)))

                 (:break e))))
  (when collided-x?
    (update-in ball [:vel 0] * -0.95))
  (when collided-y?
    (update-in ball [:vel 1] * -0.95)))

(defn system/gc [&opt entities]
  (default entities ENTITIES)
  (loop [i :down-to [(dec (length entities)) 0]
         :let [e (get entities i)]]
    (when (e :remove)
      (array/remove entities i))))


# Particle system

(defn pixel-particle/update [self dt]
  (-- (self :tick))
  (cond
    (neg? (self :tick)) (set (self :remove) true)
    (< (self :tick) 10) (set (self :color) (self :color-old)))
  (when (self :fall?)
    (update-in self [:vel 1] + 0.1))
  (update-in self [:pos 0] + (get-in self [:vel 0]))
  (update-in self [:pos 1] + (get-in self [:vel 1])))

(defn pixel-particle/draw [{:pos pos :color color}]
  (draw-pixel ;(map math/floor pos) color))

(defn pixel-particle/init [pos vel age &opt fall?]
  @{:pos @[;pos]
    :vel @[;vel]
    :fall? fall?
    :tick age
    :color PINK
    :color-old SKIN
    :update pixel-particle/update
    :draw pixel-particle/draw})

(defn pixel-particle/add [pos vel age &opt fall?]
  (array/push PARTICLES (pixel-particle/init pos vel age fall?)))

(defn spawn-trail [[x y]]
  (let [ang (* 2 PI (math/rng-uniform RNG))
        ox (* (math/cos ang) (BALL :radius) 0.2)
        oy (* (math/sin ang) (BALL :radius) 0.2)]
    (pixel-particle/add [(math/floor (+ x ox))
                         (math/floor (+ y oy))]
                        [0 0]
                        (+ 15 (math/rng-int RNG 15)))))

(defn shatter-at [pos]
  (for i 0 10
    (let [ang (* 2 PI (math/rng-uniform RNG))
          dx (* (math/cos ang) 1)
          dy (* (math/sin ang) 1)]
      (pixel-particle/add pos [dx dy] 60 true))))

(defn dust-particle/update [self dt]
  (var final-update? false)
  (let [{:friction friction :radius r :vel [vx vy] :tick tick :colors colors} self]
    (update-in self [:pos 0] + vx)
    (update-in self [:pos 1] + vy)
    (update-in self [:vel 0] * friction)
    (update-in self [:vel 1] * friction)
    (when (< tick 5)
      (/= (self :radius) 1.1)
      (when (and (zero? (% FRAME 10)) (< 1 (length colors)))
        (array/pop (self :colors))))
    (update self :tick + -1 (math/rng-uniform RNG)))

  (when (< (self :radius) 1)
    (set (self :remove) true)))

(defn dust-particle/draw [{:pos pos :radius r :colors colors :type type}]
  (match type
    0 (draw-circle-v pos r (array/peek colors))
    1 (draw-circle-lines ;(map math/round pos) r (array/peek colors))))

(defn dust-particle/init [pos]
  (def angle (+ (* (math/rng-uniform RNG) PI) PI))
  @{:pos @[;pos]
    :radius (* (math/rng-uniform RNG) 3)
    :friction 0.92
    :type (math/rng-int RNG 2)
    :tick 20
    :vel @[(math/cos angle) (math/sin angle)]
    :colors @[SKIN HEATHER PINK] # @[0x333C57FF 0x566C86FF 0x94B0C2FF 0xF4F4F4FF]
    :update dust-particle/update
    :draw dust-particle/draw})

(defn particle-system/dust-clout-at [point &opt count]
  (for i 0 (default count 8)
    (array/push PARTICLES (dust-particle/init point))))


# Screen shaker

(defn shaker/start [amplitude duration]
  (set (SHAKER :amplitude) amplitude)
  (set (SHAKER :duration) duration))

(defn shaker/update [dt]
  (let [{:duration dur :amplitude amp} SHAKER
        x-offset (math/round (- (math/rng-int RNG amp)
                                (/ amp 2)))
        y-offset (math/round (- (math/rng-int RNG amp)
                                (/ amp 2)))]
    (when (pos? dur)
      (set (CAMERA :offset) [x-offset y-offset])
      (-- (SHAKER :duration)))

    (when (zero? dur)
      (set (CAMERA :offset) [0 0]))))


# Timer (adapted from https://github.com/vrld/hump/blob/master/timer.lua)
(def TIMERS @[])

(defn timer/update [dt]
  (loop [timer :in TIMERS
         :let [{:during during :after after :limit limit} timer]]
    (set (timer :time) (+ (timer :time) dt))
    (when during
      (during dt (min (/ (timer :time) limit) 1.0)))
    (when (and (<= (timer :limit) (timer :time))
               (pos? (timer :count)))
      (if (fiber? after)
        (resume after)
        (after))
      (update timer :time - (timer :limit))
      (-- (timer :count))))
  (loop [idx :down-to [(dec (length TIMERS)) 0]
         :let [timer (get TIMERS idx)]]
    (when (zero? (timer :count))
      (array/remove TIMERS idx))))

(defn timer/during [delay during &opt after]
  (let [timer @{:time 0 :count 1 :limit delay
                :during during :after (or after noop)}]
    (array/push TIMERS timer)
    timer))

(defn timer/after [delay func]
  (timer/during delay noop func))

(defn timer/every [delay after &opt count]
  (default count math/inf)
  (let [timer @{:time 0 :limit delay :count count
                :during noop :after after}]
    (array/push TIMERS timer)
    timer))

(defn timer/script [func]
  (let [f (fiber/new func)]
    (resume f (fn [t]
                (timer/after t f)
                (yield)))))

(defn timer/tween [duration subject path target &opt method after]
  (default method linear)
  (default after noop)
  (def initial-value (get-in subject path))
  (timer/during duration
                (fn [dt t]
                  (put-in subject path
                          (lerp t initial-value target method)))))



# transition
(def TRANSITION @{:phase nil    # nil, :in, or :out
                  :percentage 0})

(defn transition/draw []
  (let [grid-size 16
        grid-size/2 (div grid-size 2)]
    (loop [i :range-to [0 (math/ceil (/ H grid-size))]
           j :range-to [0 (math/ceil (/ W grid-size))]
           :let [{:percentage p} TRANSITION
                 angle (lerp p 0 90)
                 size (lerp p 0 grid-size)
                 size/2 (div size 2)
                 rec [(+ grid-size/2 (* j grid-size))
                      (+ grid-size/2 (* i grid-size))
                      size size]]]
      (draw-rectangle-pro rec [size/2 size/2] angle PINK))))

# (do
#   (set (TRANSITION :phase) :out)
#   (timer/tween 2 TRANSITION [:percentage] 1))
# (do
#   (set (TRANSITION :phase) :in)
#   (timer/tween 2 TRANSITION [:percentage] 0))
# (timer/script (fn [wait]
#                 (set (TRANSITION :phase) :out)
#                 (timer/tween 2 TRANSITION [:percentage] 1)
#                 (wait 2)
#                 (set (TRANSITION :phase) :in)
#                 (timer/tween 2 TRANSITION [:percentage] 0)
#                 (wait 2)))


# Player

(defn player/draw [{:pos [x y] :size [w h] :life life}]
  (draw-rectangle-rounded [x y w h] 1 0 LIME)
  (for i 0 life
    (draw-rectangle-rounded [(+ 5 (* 10 i)) (- H 8) 8 2] 1 0 CHARCOAL)))

(defn player/update [self dt]
  (when (key-down? :left)
    (update-in self [:pos 0] - 2))
  (when (neg? (get-in self [:pos 0]))
    (put-in self [:pos 0] 0))
  (when (key-down? :right)
    (update-in self [:pos 0] + 2))
  (when (<= W (+ (get-in self [:pos 0]) (get-in self [:size 0])))
    (put-in self [:pos 0] (- W (get-in self [:size 0])))))

(defn player/init []
  {:pos @[(- (div W 2) (div W 20)) (div (* H 7) 8)]
   :size @[24 4]
   :life PLAYER_MAX_LIFE
   :draw player/draw
   :update player/update})



# Ball

(defn ball/draw [{:pos pos :radius r}]
  (draw-circle-v pos r BLUE))

(defn ball/update [self dt]
  # ball launching
  (when (not (self :active))
    (when (key-pressed? :space)
      (set (self :active) true)
      (put-in self [:vel 0] 0)
      (put-in self [:vel 1] -1)))

  # ball movement
  (if (self :active)
    (do
      (spawn-trail (self :pos))
      (update-in self [:pos 0] + (get-in self [:vel 0]))
      (update-in self [:pos 1] + (get-in self [:vel 1])))
    (do
      (put-in self [:pos 0] (+ (get-in PLAYER [:pos 0]) (div (get-in PLAYER [:size 0]) 2)))
      (put-in self [:pos 1] (- (div (* H 7) 8) 10))))

  # collision: ball vs walls
  (let [{:pos [x y] :radius r :vel [vx vy]} self]
    (when (or (<= W (+ x r))
              (<= (- x r) 0))
      (shaker/start 4 10)
      (particle-system/dust-clout-at [x y])
      (update-in self [:vel 0] * -1))
    (when (<= (- y r) 0)
      (shaker/start 4 10)
      (particle-system/dust-clout-at [x y])
      (update-in self [:vel 1] * -1))
    (when (<= H (+ y r))
      (put-in self [:vel 0] 0)
      (put-in self [:vel 1] 0)
      (set (self :active) false)
      (update PLAYER :life + -1)))

  # collision: ball vs player
  (let [{:pos [x y] :size [w h]} PLAYER
        {:pos [bx by] :radius br :vel [bvx bvy]} self]
    (when (check-collision-circle-rec [bx by] br [x y w h])
      (shaker/start 4 10)
      (particle-system/dust-clout-at [bx by])
      (when (pos? bvy)
        (update-in self [:vel 1] * -1)
        (put-in self [:vel 0] (div (* (- bx x (div w 2)) 5 0.9)
                                   (div w 2))))))

  # collision: ball vs bricks
  (system/collide BALL)
  )

(defn ball/init []
  {:pos @[(div W 2) (- (div (* H 7) 8) 30)]
   :vel @[0 0]
   :radius 3
   :active false
   :draw ball/draw
   :update ball/update})


#Brick

(defn brick/draw [{:pos [x y] :size [w h] :color color :hidden hidden}]
  (when (not hidden)
    (draw-rectangle-rounded [x y w h] 1.0 0 color)))

(defn brick/break [self]
  (shaker/start 4 10)
  (shatter-at (self :pos))
  (set (self :hidden) true))



# Game

(defn game/init []
  (set CAMERA (camera-2d :zoom 1))

  (array/clear TIMERS)
  (merge-into TRANSITION {:phase :in :percentage 1})
  (timer/tween 1 TRANSITION [:percentage] 0 linear
               |(set (TRANSITION :phase) nil))

  (array/clear ENTITIES)

  (merge-into PLAYER (player/init))
  (array/push ENTITIES PLAYER)

  (merge-into BALL (ball/init))
  (array/push ENTITIES BALL)

  (def initial-down-position 10)
  (for i 0 LINES_OF_BRICKS
    (for j 0 BRICKS_PER_LINE
      (let [target-y (+ (* i BRICK_H) (* i 2) initial-down-position)]
        (def brick @{:pos @[(+ (math/round (* (inc j) BRICK_GAP)) (* j BRICK_W))
                           (- 0 (math/rng-int RNG 100))]
                     :size @[BRICK_W BRICK_H]
                     :color (if (zero? (% (+ j i) 2))
                              YELLOW
                              ORANGE)
                     :break brick/break
                     :hidden false
                     :draw brick/draw
                     :update noop})
        (array/push ENTITIES brick)
        (timer/tween 1 brick [:pos 1] target-y (out elastic))))))

(defn game/over? []
  (cond
    (<= (PLAYER :life) 0) (set GAMEOVER? true)
    (system/all ENTITIES [:break] |($ :hidden)) (set GAMEOVER? true)
    nil))

(defn game/update [dt]
  (timer/update dt)
  (shaker/update dt)

  (when GAMEOVER?
    (when (key-pressed? :enter)
      (game/init)
      (set GAMEOVER? false)))

  (when (not GAMEOVER?)
    (when (key-pressed? :p)
      (set PAUSED? (not PAUSED?)))

    (when (not PAUSED?)
      (system/update dt)
      (system/update dt PARTICLES)
      (system/gc PARTICLES)
      (game/over?))))

(defn game/draw []
  (clear-background BLACK)
  (when (not GAMEOVER?)
    (system/draw PARTICLES)
    (system/draw)

    (when PAUSED?
      (draw-text "GAME PAUSED"
                 (- (div W 2)
                    (div (measure-text "GAME PAUSED" FONT_SIZE) 2))
                 (- (div H 2) FONT_SIZE)
                 FONT_SIZE ORANGE))

    (when (not= nil (TRANSITION :phase))
      (transition/draw)))

  (when GAMEOVER?
    (draw-text "PRESS [ENTER] TO PLAY AGIAN"
               (- (div W 2)
                  (div (measure-text "PRESS [ENTER] TO PLAY AGIAN" FONT_SIZE) 2))
               (- (div H 2) 50)
               FONT_SIZE YELLOW)))

(defn start []
  (set-trace-log-level :none)
  (init-window SCREEN_W SCREEN_H "game")
  (def canvas (load-render-texture W H))
  (set-texture-filter (get-render-texture-texture2d canvas) :point)
  (game/init)
  (set-target-fps 60)

  (while (not (window-should-close))
    (ev/sleep 0)
    (game/update (get-frame-time))
    (begin-texture-mode canvas)
    (game/draw)
    (end-texture-mode)
    (begin-drawing)
    (clear-background BLACK)
    (begin-mode-2d CAMERA)
    (draw-texture-pro (get-render-texture-texture2d canvas)
                      [0 0 W (* -1 H)]
                      [0 0 SCREEN_W SCREEN_H]
                      [0 0] 0 :white)
    (when DEV?
      (draw-fps 10 10))
    (end-mode-2d)
    (end-drawing)

    (++ FRAME))

  (unload-render-texture canvas)
  (close-window))

(defn main [&]
  (start))

# (def game (ev/call start))
# (start)
