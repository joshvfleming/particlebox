(ns physim.core
  (:require [THREE :as THREE]))

(def particle-radius 20)
(def particle-diameter (* 2 particle-radius))
(def particle-count 10)
(def gravity-multiplier 200)
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

(defn generate-particles
  [n]
  (for [i (range n)]
    { :pos [(rand-x) (rand-y) (rand-y)]
      :vel [(/ (* initial-speed (rand-x)) scene-width)
            (/ (* initial-speed (rand-y)) scene-width)
            (/ (* initial-speed (rand-y)) scene-width)] }))

(defn set-position!
  [mesh [x y z]]
  (set! (.-x (.-position mesh)) x)
  (set! (.-y (.-position mesh)) y)
  (set! (.-z (.-position mesh)) z))

(defn rotate-angles!
  [mesh [x y z]]
  (set! (.-x (.-rotation mesh)) (+ (.-x (.-rotation mesh)) x))
  (set! (.-y (.-rotation mesh)) (+ (.-y (.-rotation mesh)) y))
  (set! (.-z (.-rotation mesh)) (+ (.-z (.-rotation mesh)) z)))

(defn mesh-from-particle
  [p]
  (let [geometry (THREE.SphereGeometry.
                  particle-radius particle-radius particle-radius)
        material (THREE.MeshNormalMaterial.)
        mesh (THREE.Mesh. geometry material)]
    (set-position! mesh (:pos p))
    mesh))

(defn init-particles!
  [ps]
  (reset! particles ps)
  (reset! particle-meshes (map mesh-from-particle ps)))

(defn make-renderer!
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
  [a b]
  (magnitude (map - a b)))

(defn calc-accel
  "Calculates the sum vector of the gravitational force exerted on the partical
by all the other particles."
  [p all-particles]
  (reduce
   (fn [a n]
     (let [dir (direction-from p (:pos n))
           dist (distance p (:pos n))]
       (map + a
            (if (> dist 0)
              (direction dir (/ gravity-multiplier
                                (* dist dist)))
              nullv))))
   nullv
   all-particles))

(defn move
  "Moves the particle according to the universe's equations of motion."
  [p v all]
  (let [a (calc-accel p all)
        v (map + v a)
        speed (magnitude v)
        v (if (> speed speed-limit)
            (direction v speed-limit)
            v)
        p (map + p v)]
    [p v a]))

(defn collided?
  [a b]
  (< (distance (:pos a) (:pos b)) particle-diameter))

(defn handle-collisions!
  []
  (doseq [a (range particle-count)
          b (range particle-count)]
    (let [pa (nth @particles a)
          pb (nth @particles b)]
      (when (and (not= a b) (collided? pa pb))
        (let [dir (direction-from (:pos pb) (:pos pa))
              dist (distance (:pos pa) (:pos pb))
              bounce (direction dir (- particle-diameter dist))]
          (swap! particles
                 assoc
                 a (assoc pa
                     :pos (map + (:pos pa) bounce)
                     :vel (map +
                               (:vel pa)
                               (direction dir (magnitude (:vel pa)))))
                 b (assoc pb
                     :pos (map - (:pos pb) bounce)
                     :vel (map -
                               (:vel pb)
                               (direction dir (magnitude (:vel pb)))))))))))

(defn motion-tick!
  []
  (doseq [i (range particle-count)]
    (let [p (nth @particles i)
          [p v a] (move (:pos p) (:vel p) @particles)]
      (reset! particles
              (assoc (vec @particles) i {:pos p :vel v})))))

(init-particles! (generate-particles particle-count))
(def render-scene (make-renderer! @particle-meshes))  
(defn render
  []
  (motion-tick!)
  (handle-collisions!)

  (doseq [i (range particle-count)]
    (let [p (nth @particles i)
          m (nth @particle-meshes i)]
      ;; static rotation for blinginess
      (rotate-angles! m [0 2 0])
      (set-position! m (:pos p))))

  (render-scene))
 
(defn animate
  []
  (.requestAnimationFrame js/window animate)
  (render))

(animate)
