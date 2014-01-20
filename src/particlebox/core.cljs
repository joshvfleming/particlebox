;; Particle box, a physics playground.
(ns particlebox.core)

(def particle-radius 20)
(def particle-diameter (* 2 particle-radius))
(def particle-count 10)
(def gravitational-constant 200)
(def speed-limit 5)
(def initial-speed 0)

(def scene-width window/innerWidth)
(def scene-height window/innerHeight)

(def particles (atom []))
(def particle-meshes (atom []))
(def collided-particles (atom {}))

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

(defn direction
  "Given a vector, returns a unit vector in the same direction, optionally
scaled."
  ([v] (direction v 1))
  ([v m]
     (let [mag (magnitude v)]
       (map #(* (/ % mag) m) v))))

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

        ;; added a speed limit here because gavitational sigularities were
        ;; causing particles to become superaccelerated, which is kind of cool
        ;; but generally doesn't make for a good simulation. This is needed only
        ;; when there's no collision detection.
        speed (magnitude v)
        v (if (> speed speed-limit)
            (direction v speed-limit)
            v)

        p (map + p v)]
    [p v a]))

(defn collided?
  "Determines whether the two particles have collided."
  [a b]
  (< (distance (:pos a) (:pos b)) particle-diameter))

(defn handle-collisions!
  "Scans all particles for collisions, and when any two particles collide,
bounces them away from one another and cancels out velocity along the shared
axis. This is not quite right, but suffices for now."
  []
  (doseq [a (range particle-count)
          b (range particle-count)]
    (let [pa (nth @particles a)
          pb (nth @particles b)]
      (when (and (not= a b) (collided? pa pb))
        (let [dir (direction-from (:pos pa) (:pos pb))
              dist (distance (:pos pa) (:pos pb))
              bounce (direction dir (- particle-diameter dist))]
          (swap! particles
                 assoc
                 a (assoc pa
                     :pos (map - (:pos pa) bounce)
                     :vel (map -
                               (:vel pa)
                               (direction dir (magnitude (:vel pa)))))
                 b (assoc pb
                     :pos (map + (:pos pb) bounce)
                     :vel (map +
                               (:vel pb)
                               (direction dir (magnitude (:vel pb)))))))))))

(defn motion-tick!
  "Performs all motion in the universe for one stroboscopic 'tick' of the
clock."
  []
  (doseq [i (range particle-count)]
    (let [p (nth @particles i)
          [p v a] (move (:pos p) (:vel p) @particles)]
      (swap! particles
             assoc i {:pos p :vel v}))))

(defn generate-particles
  "Creates the given number of particles, and arranges them in the universe
randomly."
  [n]
  (vec
   (for [i (range n)]
     {:pos [(rand-x) (rand-y) (rand-y)]
      :vel (direction [(rand-x) (rand-y) (rand-y)] initial-speed)})))

(defn render
  [drawfn]
  (motion-tick!)
  (handle-collisions!)

  (doseq [i (range particle-count)]
    (let [p (nth @particles i)
          m (nth @particle-meshes i)]
      ;; static rotation for blinginess
      (rotate-angles! m [0 2 0])

      (set-position! m (:pos p))))

  (drawfn))
 
(defn animate
  [drawfn]
  (.requestAnimationFrame js/window (partial animate drawfn))
  (render drawfn))

(defn ^:export start
  []
  (init-particles! (generate-particles particle-count))
  (animate (make-renderer! @particle-meshes)))
