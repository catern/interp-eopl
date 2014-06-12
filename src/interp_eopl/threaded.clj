(ns interp-eopl.conc)

; the syntax of the language is SIMILAR to the ast, but actually completely distinct
(defn make-ast [val]
  (cond
   (number? val) {:type :number :value val}
   (or (true? val) (false? val)) {:type :boolean :value val}
   (= clojure.lang.Atom (class val)) {:type :atom :value val}
   (string? val) {:type :string :value val}
   (symbol? val) {:type :symbol :value val}
   (seq? val) {:type :list :value (map make-ast val)}
   (fn? val) {:type :procedure :primitive val}
   (nil? val) {:type :number :value 0}
   :else (throw (Throwable. "Unknown type"))))

(declare apply)
(declare eval)

(defn val-to-var [val]
  {:recursive false :value val})

(defn extend-env [env name val]
  (if (and (coll? name) (coll? val))
    (conj env (zipmap name (map val-to-var val)))
    (conj env (hash-map name (val-to-var val)))))

(defn val-to-var-recursive [val]
  {:recursive true :value val})

; for a recursive value, the value must be constructed at lookup-time
(defn extend-env-recursive [env name val]
  (if (and (coll? name) (coll? val))
    (conj env (zipmap name (map val-to-var-recursive val)))
    (conj env (hash-map name (val-to-var-recursive val)))))

(defn lookup-in-env [env symbol]
  (let [var (some symbol env)
        recursive (:recursive var)
        val (:value var)]
    (if recursive
      (conj val {:env env})
      val)))

(defn wrap-clojure-primitive [primitive]
  (fn [args] 
  (make-ast (clojure.core/apply primitive (map :value args)))))

(defn make-mutex [_]
  {:type :mutex
   :waitlist (java.util.concurrent.LinkedBlockingQueue.)
   :open (atom true)})

(defn set-atom [[atom-val value-val]]
  (reset! (:value atom-val) value-val))

(defn get-atom [[atom-val]]
  (deref (:value atom-val)))

(defn make-atom [_]
  (make-ast (atom (make-ast 0))))

(def default-env 
  (extend-env (list)
              ['+ 
               '*
               '-
               '=
               'println
               'spawn
               'set
               'get
               'atom
               'mutex
               'do
               ] 
              [(make-ast (wrap-clojure-primitive +))
               (make-ast (wrap-clojure-primitive *))
               (make-ast (wrap-clojure-primitive -))
               (make-ast (wrap-clojure-primitive =))
               (make-ast (wrap-clojure-primitive println))
               (make-ast spawn-program-thread)
               (make-ast set-atom)
               (make-ast get-atom)
               (make-ast make-atom)
               (make-ast make-mutex)
               (make-ast last) ; corresponds cleverly to do
               ]
              ))

(def special-symbol-set 
  '#{if fn let let-rec lock unlock})

(defn special-symbol [{:keys [type value]}]
  (and (= type :symbol)
       (contains? special-symbol-set value)
       value))

(defn falsey-val? [{:keys [type value]}] 
  (or 
   (and (= type :number) (= value 0))
   (and (= type :boolean) (= value false))))

;; queue operations

(defn empty-queue? [q]
  (= (.peek q) nil))

;; (add-to-queue! q e)
(defn add-to-queue! [q e]
  (.put q e))

;; (pop-from-queue! q)
(defn pop-from-queue! [q] 
  (.remove q))

;; concurrency
(def run-queue (java.util.concurrent.LinkedBlockingQueue.))
(def default-thread-runtime 50)
(def current-thread-time-remaining (atom default-thread-runtime))

(defn add-to-run-queue! [thunk]
  (.put run-queue thunk))

(defn pop-from-run-queue! []
;  (.take run-queue)) ; blocks
  (.remove run-queue)) ; throws exception

(defn empty-run-queue? []
  (= (.peek run-queue) nil))

(defn run-next-thread! []
  (when-not (empty-run-queue?)
    (reset! current-thread-time-remaining default-thread-runtime)
    (force (pop-from-run-queue!))))

;; TODO switch from procedure continuations to data structure continuations
(defn cont-dispatch [expr-val cont]
  (if (fn? cont)
    (do 
      ;; (println "CONT-DISPATCH:")
      ;; (print "expr-val: ")
      ;; (println expr-val)
      ;; (print "expr-val: ")
      ;; (println cont)
      ;; (try (throw (Exception. "")) (catch Exception e (.printStackTrace e *out*)))
    (cont expr-val))
    (let [[cont-type saved-cont] cont]
      (case saved-cont
        (cont expr-val)))))

(defn execute-cont [cont expr-val]
  (if (= @current-thread-time-remaining 0)
    (do (add-to-run-queue! (delay (cont-dispatch expr-val cont)))
        (run-next-thread!))
    (do (swap! current-thread-time-remaining dec)
        (cont-dispatch expr-val cont))))

(defn end-subthread-cont [val]
  (do 
    (trampoline identity val)
    (run-next-thread!)))

(defmulti eval
  (fn [{:keys [type value] :as foo} env cont] 
        (if (= :list type) 
          (or (special-symbol (first value)) :application)
          type)))

; self-evaluating types
(defmethod eval :number [expr env cont] (execute-cont cont expr))
(defmethod eval :boolean [expr env cont] (execute-cont cont expr))
(defmethod eval :string [expr env cont] (execute-cont cont expr))
(defmethod eval :procedure [expr env cont] (execute-cont cont expr))
(defmethod eval :atom [expr env cont] (execute-cont cont expr))
(defmethod eval :mutex [expr env cont] (execute-cont cont expr))

; variable lookup
(defmethod eval :symbol [expr env cont] (execute-cont 
                                         cont 
                                         (lookup-in-env env (:value expr))))

;; not using refs and transactions because one change to open
;; is already atomic
;; whatever, TODO make this atomic later
(defmethod eval 'lock [{:keys [value]} env cont]
  (eval (second value) env 
        (fn [{:keys [open waitlist]}]
            (if @open
              (do 
                (reset! open false)
                (execute-cont cont (make-ast 0)))
              (do
                (add-to-queue! waitlist (delay (execute-cont cont (make-ast 0))))
                (run-next-thread!))))))

;; TODO make this atomic later
(defmethod eval 'unlock [{:keys [value]} env cont]
  (eval (second value) env 
        (fn [{:keys [open waitlist]}]
            (when (= @open false)
              (if (empty-queue? waitlist)
                  (reset! open true)
                  (add-to-run-queue! (pop-from-queue! waitlist))))
            (execute-cont cont (make-ast 0)))))
    

(defn spawn-program-thread [[fn-val]]
  (do 
    ;; add suspended execution to queue
    (add-to-run-queue!
     (delay (apply fn-val (list (make-ast 42)) end-subthread-cont)))
    ;; return 0
    (make-ast 0)))

; special forms
(defmethod eval 'if [{[_ pred-expr true-expr false-expr] :value} env cont] 
  (eval pred-expr env (fn [predicate] 
                        (if (not (falsey-val? predicate))
                          (eval true-expr env cont)
                          (eval false-expr env cont)))))

;; TODO create a check-unbound function to make a list of free vars in body
;; then construct an environment with only those in it
(defmethod eval 'fn [{[_ {args :value} body] :value} env cont]
  (execute-cont 
   cont 
   {:type :procedure, :args (map :value args), :body body, :env env}))

(defmethod eval 'let
  [{[_ {bindings :value} body] :value} env cont]
  (fn []
    (let [names (map :value (take-nth 2 bindings))
          ;; the list of values to be bound, each in expression form
          bindval-exprs (take-nth 2 (rest bindings))]
      (eval-expr-list-full bindval-exprs env 
                      #(eval body (extend-env env names %) cont)))))

(defmethod eval 'let-rec 
  [{[_ {name :value} {args :value} fun-body body] :value} env cont]
  (fn []
   (eval body 
         (extend-env-recursive 
          env
          name
          {:type :procedure, :args (map :value args), :body fun-body})
         cont)))

;; TODO: make a continuation that evaluates the operator-expr, and pass
;; in the operator-expr instead of the operator-val
;; ^ that doesn't make sense
;; TODO this code is ugly
(defn eval-expr-list [exprs env cont vals]
  (if (empty? exprs)
    (fn [last-val] 
      (cont (reverse (conj vals last-val))))
    (fn [next-val] 
      (eval (first exprs) env 
            (eval-expr-list (rest exprs)
                            env
                            cont
                            (conj vals next-val))))))

(defn eval-expr-list-full [exprs env cont]
  (if (empty? exprs)
    (cont (list))
    (eval (first exprs) env 
          (eval-expr-list (rest exprs) env cont (list)))))

(defmethod eval :application [{[operator & operands] :value} env cont] 
  (fn []
    (eval operator env 
          (fn [operator-val]
            (eval-expr-list-full operands env #(apply operator-val % cont))))))

;; (defmethod eval :application [{[operator & operands] :value} env cont] 
;;   (fn []
;;     (eval operator env 
;;           (if (empty? operands)
;;             #(apply % (list) cont)
;;             (fn [operator-val] 
;;               (eval (first operands) env
;;                     (eval-expr-list (rest operands)
;;                                     env
;;                                     #(apply operator-val % cont)
;;                                     '())))))))

(defn apply [{:keys [primitive args body env] :as rator} operands cont]
  (if primitive 
   (execute-cont cont (primitive operands))
  ; (make-ast (clojure.core/apply primitive (reverse (map :value operands)))))
   (eval body (extend-env env args operands) cont)))

(defn eval-program [body]
  (reset! current-thread-time-remaining default-thread-runtime)
  (while (not (empty-run-queue?)) (pop-from-run-queue!))
  (trampoline eval (make-ast body) default-env identity))
;; equiv to (eval (make-ast body) default-env (partial trampoline identity))

(defn racey-program [iteration1 iteration2]
  (eval-program '(let (x (atom))
     (let-rec f (n) (if (= n 0) 
                      (get x) 
                      (+ (* 0 (set x (+ 1 (get x)))) 
                         (f (- n 1))))
              (+ (spawn (fn (d) (f iteration1)))
                 (f iteration2))))))

(defn racey-program [iter1 iter2 iter3]
   (eval-program `(~'let ~'(x (atom) m (mutex))
      (~'let-rec ~'f ~'(n) ~'(if (= n 0) 
                       (get x) 
                       (+ 
                          (* 0 (set x (+ 1 (get x)))) 
                        
                          (f (- n 1))))
               (~'+ (~'spawn (~'fn ~'(d) (~'f ~iter1)))
                    (~'spawn (~'fn ~'(d) (~'f ~iter2)))
                  (~'f ~iter3))))))

(defn safe-program [iter1 iter2 iter3]
   (eval-program `(~'let ~'(x (atom) m (mutex))
      (~'let-rec ~'f ~'(n) ~'(if (= n 0) 
                       (get x) 
                       (+ (lock m) 
                          (* 0 (set x (+ 1 (get x)))) 
                          (unlock m)
                          (f (- n 1))))
               (~'+ (~'spawn (~'fn ~'(d) (~'f ~iter1)))
                    (~'spawn (~'fn ~'(d) (~'f ~iter2)))
                  (~'f ~iter3))))))
