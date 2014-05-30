(ns interp-eopl.cont)

; the syntax of the language is SIMILAR to the ast, but actually completely distinct
(defn make-ast [val]
  (cond
   (number? val) {:type :number :value val}
   (or (true? val) (false? val)) {:type :boolean :value val}
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

(def default-env 
  (extend-env (list)
              ['+ 
               '*
               '-
               '=
               'println
               ] 
              [(make-ast +) 
               (make-ast *)
               (make-ast -)
               (make-ast =)
               (make-ast println)
               ]
              ))

(defn special-symbol [{:keys [type value]}]
  (and (= type :symbol) 
       (or (special-symbol? value) 
           (= value 'fun)
           (= value 'fn)
           (= value 'let-rec))
       value))

(defn falsey-val? [{:keys [type value]}] 
  (or 
   (and (= type :number) (= value 0))
   (and (= type :boolean) (= value false))))

(defmulti eval
  (fn [{:keys [type value] :as foo} env cont] 
        (if (= :list type) 
          (or (special-symbol (first value)) :application)
          type)))

; self-evaluating types
(defmethod eval :number [expr env cont] (cont expr))
(defmethod eval :boolean [expr env cont] (cont expr))
(defmethod eval :string [expr env cont] (cont expr))
(defmethod eval :procedure [expr env cont] (cont expr))

; variable lookup
(defmethod eval :symbol [expr env cont] (cont (lookup-in-env env (:value expr))))

; special forms
(defmethod eval 'if [{[_ pred-expr true-expr false-expr] :value} env cont] 
  (eval pred-expr env (fn [predicate] 
                        (if (not (falsey-val? predicate))
                          (eval true-expr env cont)
                          (eval false-expr env cont)))))

;; TODO create a check-unbound function to make a list of free vars in body
;; then construct an environment with only those in it
(defmethod eval 'fn [{[_ {args :value} body] :value} env cont]
  (cont {:type :procedure, :args (map :value args), :body body, :env env}))

(defmethod eval 'let-rec 
  [{[_ {name :value} {args :value} fun-body body] :value} env cont]
  (do 
    (print "Name: ")
    (println name)
   (eval body 
         (extend-env-recursive 
          env
          name
          {:type :procedure, :args (map :value args), :body fun-body})
         cont)))

(defn map-cont [f list cont processed]
  (if (empty? list)
    (fn [x] (cont processed) 
    (f (first list) (fn [x] (map-cont f (rest list) cont (cons x processed)))))))

;; TODO: make a continuation that evaluates the operator-expr, and pass
;; in the operator-expr instead of the operator-val
(defn eval-operands-list [operands env operator-val cont operand-vals]
  (if (empty? operands)
    (fn [last-operand-val] 
      (apply operator-val 
             (conj operand-vals last-operand-val) 
             cont))
    (fn [next-operand-val] 
      (eval (first operands) env 
            (eval-operands-list (rest operands)
                                env
                                operator-val
                                cont
                                (conj operand-vals next-operand-val))))))

(defn eval-operands-list-initial-cont [operands env cont]
  (if (empty? operands)
    (fn [operator-val]
      (apply operator-val
             (list)
             cont))
    (fn [operator-val] 
      (eval (first operands) env
            (eval-operands-list (rest operands)
                                env
                                operator-val
                                cont
                                (list))))))

;; we hack this by using (partial trampoline identity)
;; as the continuation to get the value of the operands
(defmethod eval :application [{:keys [value]} env cont] 
  (eval (first value) env 
        (fn [operator-val] (apply
                            operator-val
                            (map #(eval % env (partial trampoline identity)) 
                                 (rest value))
                            cont))))

(defmethod eval :application [{:keys [value]} env cont] 
  (eval (first value) env 
        (eval-operands-list-initial-cont (rest value) 
                                         env 
                                         cont)))

(defn apply [{:keys [primitive args body env]} operands cont]
  (if primitive
    ;; the operands should all be primitive types, so we can safely map :value and apply
    (cont (make-ast (clojure.core/apply primitive (reverse (map :value operands)))))
    (eval body (extend-env env args operands) cont)))


(defn eval-program [body]
  (trampoline eval (make-ast body) default-env identity))
;; equiv to (eval (make-ast body) default-env (partial trampoline identity))
