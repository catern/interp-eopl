(ns interp-eopl.basic)

; the syntax of the language is SIMILAR to the ast, but actually completely distinct
(defn make-ast [val]
  (cond
   (number? val) {:type :number :value val}
   (or (true? val) (false? val)) {:type :boolean :value val}
   (string? val) {:type :string :value val}
   (symbol? val) {:type :symbol :value val}
   (seq? val) {:type :list :value (map make-ast val)}
   (fn? val) (throw (Throwable. "Need make-proc to make procs"))
   :else (throw (Throwable. "Unknown type"))))

(defn make-proc [val operandtype returntype]
   {:type :procedure :takes operandtype :returns returntype :primitive val})

(declare apply)
(declare eval)

(defn val-to-var [val]
  {:recursive false :value val})

(defn extend-env [env name val]
  (if (and (coll? name) (coll? val))
    (conj env (zipmap name (map val-to-var val)))
    (conj env (hash-map name (val-to-var val)))))

(def default-env 
  (extend-env (list)
              ['+ 
               '=
               ] 
              [(make-proc + :number :number)
               (make-proc = :number :number)
               ]
              ))

; for a recursive value, the value must be constructed at lookup-time
(defn lookup-in-env [env symbol]
  (:value (some symbol env)))

(defn special-symbol [{:keys [type value]}]
  (and (= type :symbol) 
       (or (special-symbol? value) (= value 'fn))
       value))

(defn falsey-val? [{:keys [type value]}] 
  (or 
   (and (= type :number) (= value 0))))

(defmulti eval
  (fn [{:keys [type value]} env] 
      (if (= :list type) 
        (or (special-symbol (first value)) :application)
        type)))

; self-evaluating types
(defmethod eval :number [expr env] expr)
(defmethod eval :string [expr env] expr)
(defmethod eval :procedure [expr env] expr)

; variable lookup
(defmethod eval :symbol [expr env] (lookup-in-env env (:value expr)))

; special forms
(defmethod eval 'if [{[_ pred-expr true-expr false-expr] :value} env] 
  (if (not (falsey-val? (eval pred-expr env)))
    (eval true-expr env)
    (eval false-expr env)))

;; TODO create a check-unbound function to make a list of free vars in body
;; then construct an environment with only those in it
(defmethod eval 'fn [{[_ {args :value} body] :value} env]
  {:type :procedure, :args (map :value args), :body body, :env env})

(defmethod eval :application [{:keys [value]} env] 
    (let [[operator & operands] (map #(eval % env) value)]
      (apply operator operands)))

(defn apply [{:keys [primitive args body env]} operands]
  (if primitive
    ;; the operands should all be primitive types, so we can safely map :value and apply
    (make-ast (clojure.core/apply primitive (map :value operands)))
    (eval body (extend-env env args operands))))

(defn eval-program [body]
  (eval (make-ast body) default-env))
