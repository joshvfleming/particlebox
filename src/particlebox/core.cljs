;; Particle box, a physics playground.
(ns particlebox.core)

(def particle-radius 20)
(def particle-diameter (* 2 particle-radius))
(def particle-count 10)
(def gravitational-constant 200)
(def initial-speed 0)
(def bounciness 0.4)

(def scene-width window/innerWidth)
(def scene-height window/innerHeight)

(def particles (atom []))
(def particle-meshes (atom []))

(def nullv [0 0 0])

(defn rand-x
  []
  (- (rand-int scene-width)
     (/ scene-width 2)))

(defn rand-y
  []
  (- (rand-int scene-height)
     (/ scene-height 2)))

(defn set-position!
  "Convenience function for changing the position of an object. This just wraps
some of the three.js javascriptiness."
  [mesh [x y z]]
  (set! (.-x (.-position mesh)) x)
  (set! (.-y (.-position mesh)) y)
  (set! (.-z (.-position mesh)) z))

(defn rotate-angles!
  "Convenience function for rotating an object. This just wraps some of the
three.js javascriptiness."
  [mesh [x y z]]
  (set! (.-x (.-rotation mesh)) (+ (.-x (.-rotation mesh)) x))
  (set! (.-y (.-rotation mesh)) (+ (.-y (.-rotation mesh)) y))
  (set! (.-z (.-rotation mesh)) (+ (.-z (.-rotation mesh)) z)))

(defn mesh-from-particle
  "Builds a three.js mesh from the particle."
  [p]
  (let [geometry (THREE.SphereGeometry.
                  particle-radius particle-radius particle-radius)
        material (THREE.MeshNormalMaterial.)
        mesh (THREE.Mesh. geometry material)]
    (set-position! mesh (:pos p))
    mesh))

(defn init-particles!
  "Sets up all the necessary simulation state for the particles."
  [ps]
  (reset! particles ps)
  (reset! particle-meshes (map mesh-from-particle ps)))

(defn make-renderer!
  "Creates a renderer function from the meshe objects. The resulting function
has no parameters, and completes an entire redraw of the scene when it is
called."
  [meshes]
  (let [camera (THREE.PerspectiveCamera.
                50 (/ scene-width
                      scene-height) 1 10000)
        scene (THREE.Scene.)
        renderer (THREE.CanvasRenderer.)]
    (set! (.-z (.-position camera)) 1000)
    (doseq [m meshes]
      (.add scene m))
    (.setSize renderer scene-width scene-height)
    (.appendChild (.-body js/document) (.-domElement renderer))

    (fn []
      (.render renderer scene camera))))

(defn magnitude
  "Calculates the magnitude (length) of a vector."
  [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn scale
  [v s]
  (map #(* % s) v))

(defn direction
  "Given a vector, returns a unit vector in the same direction, optionally
scaled."
  ([v] (direction v 1))
  ([v m]
     (let [mag (magnitude v)]
       (if (zero? mag)
         nullv
         (map #(* (/ % mag) m) v)))))

(defn project
  "Calculates the vector projection of a in the direction of b."
  [a b]
  (let [scalar (apply + (map * a (direction b)))]
    (direction b scalar)))

(defn direction-from
  "Given two vectors, returns a unit vector representing the direction from the
first to the second, optionally scaled."
  ([a b] (direction-from a b 1))
  ([a b m]
     (direction (map - b a) m)))

(defn distance
  "Calculates the distance betwen two points."
  [a b]
  (magnitude (map - a b)))

(defn calc-accel
  "Calculates the sum vector of the gravitational force exerted on the particle
by all the other particles."
  [p all-particles]
  (reduce
   (fn [a n]
     (let [dir (direction-from p (:pos n))
           dist (distance p (:pos n))]
       (map + a
            (if (> dist 0)
              (direction dir (/ gravitational-constant
                                (* dist dist)))
              nullv))))
   nullv
   all-particles))

(defn move
  "Moves the particle according to the universe's equations of motion."
  [p v all]
  (let [a (calc-accel p all)
        v (map + v a)
        p (map + p v)]
    [p v a]))

(defn particle
  "Creates a particle from the set of position, velocity, and acceleration
vectors."
  [[p v a]]
  {:pos p :vel v :acc a})

(defn update-universe
  "Performs all motion in the universe for one stroboscopic 'tick' of the
clock."
  [particles]
  (vec
   (map #(particle (move (:pos %) (:vel %) particles))
        particles)))

(defn collided?
  "Determines whether the two particles have collided."
  [a b]
  (< (distance (:pos a) (:pos b)) particle-diameter))

(defn detect-collisions
  "Detects collisions in the universe, and returns them as pairs of index and
particle."
  [particles]
  (for [a (range particle-count)
        b (range particle-count)
        :let [pa (nth particles a)
              pb (nth particles b)]
        :when (and (not= a b) (collided? pa pb))]
    [a pa b pb]))

(defn handle-collisions
  "Scans the particles for collisions, and when any two particles collide,
bounces them away from one another and cancels out velocity along the shared
axis. This is not quite right, but suffices for now."
  [particles]
   (let [collisions (detect-collisions particles)]
     (if (empty? collisions)
       particles

       ;; if there are collisions, calculate the appropriate adjustments to
       ;; position and velocity.
       (reduce
        (fn [ps [a-idx a-key a-fn
                 b-idx b-key b-fn]]
          (-> ps
              (update-in [a-idx a-key] a-fn)
              (update-in [b-idx b-key] b-fn)))

        particles

        (for [[a pa b pb] collisions
              :let [dir (direction-from (:pos pa) (:pos pb))
                    dist (distance (:pos pa) (:pos pb))
                    bounce (direction dir (/ (- particle-diameter dist) 2))]]
          [a :pos #(map - % bounce)
           a :vel #(map -
                        %
                        (scale (project % dir) bounciness))
           b :pos #(map + % bounce)
           b :vel #(map -
                        %
                        (scale (project % (direction dir -1)) bounciness))])))))

(defn generate-particles
  "Creates the given number of particles, and arranges them in the universe
randomly."
  [n]
  (vec
   (for [i (range n)]
     {:pos [(rand-x) (rand-y) (rand-y)]
      :vel (direction [(rand-x) (rand-y) (rand-y)] initial-speed)})))

(defn update-graphics-meshes!
  "Update the graphics meshes to be in sync with the current state of the
universe."
  [particles]
  (doseq [i (range particle-count)]
    (let [p (nth particles i)
          m (nth @particle-meshes i)]

      ;; static rotation for blinginess
      (rotate-angles! m [0 2 0])

      (set-position! m (:pos p)))))

(defn render
  [drawfn]
  (let [updated-particles
        (-> @particles
            update-universe
            handle-collisions)]

    (reset! particles updated-particles)

    (update-graphics-meshes! updated-particles)
    (drawfn)))
 
(defn animate
  [drawfn]
  (.requestAnimationFrame js/window (partial animate drawfn))
  (render drawfn))

(defn ^:export start
  []
  (init-particles! (generate-particles particle-count))
  (animate (make-renderer! @particle-meshes)))
