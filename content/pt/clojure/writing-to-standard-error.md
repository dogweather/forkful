---
title:    "Clojure: Escrevendo para o erro padrão"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por que usar a escrita para o erro padrão?

A escrita para o erro padrão é um recurso importante para programadores que desejam identificar erros e solucioná-los de maneira eficiente em suas aplicações. Ao escrever para o erro padrão, é possível obter informações detalhadas sobre os erros que ocorrem durante a execução do código, facilitando a identificação e resolução de problemas.

# Como fazer:

Para escrever para o erro padrão em Clojure, é necessário importar a biblioteca "clojure.pprint". Em seguida, utilizando a função "pprint", é possível escrever mensagens de erro formatadas de maneira clara e legível para o usuário. Veja abaixo um exemplo de código e a saída gerada:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(defn divide [x y]
  (if (= y 0)
    (throw (ex-info "Não é possível dividir por zero!" {}))
    (println "O resultado da divisão é:" (/ x y))))

(try
  (divide 10 0)
  (catch Exception e
    (pprint e)))

```

```
Não é possível dividir por zero!
{:cause "Divide by zero", :via [{:type java.lang.ArithmeticException, :message "Divide by zero", :at [clojure.lang.Numbers divide "Numbers.java" 188]}], :trace [[clojure.lang.Numbers divide "Numbers.java" 188] [user$divide__1644 invokeStatic "NO_SOURCE_FILE" 16] [user$divide__1644 invoke "NO_SOURCE_FILE" 16] [clojure.lang.Compiler eval "Compiler.java" 7177] [clojure.lang.Compiler eval "Compiler.java" 7167] [clojure.core$eval__10164 invokeStatic "core.clj" 3172] [clojure.core$eval__10164 invoke "core.clj" 3172] [clojure.main$repl$read_eval_print__9112$fn__9115 invoke "main.clj" 437] [clojure.main$repl$read_eval_print__9112 invoke "main.clj" 436] [clojure.main$repl$fn__9121 invoke "main.clj" 455] [clojure.main$repl invokeStatic "main.clj" 455] [clojure.main$repl doInvoke "main.clj" 317] [clojure.lang.RestFn invoke "RestFn.java" 1523] [clojure.tools.nrepl.middleware.interruptible_eval$evaluate$fn__826 invoke "interruptible_eval.clj" 87] [clojure.tools.nrepl.middleware.interruptible_eval$evaluate invokeStatic "interruptible_eval.clj" 87] [clojure.tools.nrepl.middleware.interruptible_eval$evaluate invoke "interruptible_eval.clj" 72] [clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__847$fn__850 invoke "interruptible_eval.clj" 222] [clojure.core$comp$fn__5849 invoke "core.clj" 2569] [clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__841 invoke "interruptible_eval.clj" 190] [clojure.lang.AFn run "AFn.java" 22] [java.util.concurrent.ThreadPoolExecutor runWorker "ThreadPoolExecutor.java" 1142] [java.util.concurrent.ThreadPoolExecutor$Worker run "ThreadPoolExecutor.java" 617] [java.lang.Thread run "Thread.java" 745]]}
```

# Mergulho Profundo:

Além da função "pprint", também é possível utilizar a função "println" para escrever para o erro padrão. No entanto, é importante notar que esta função não formata as mensagens de erro, tornando-as mais difíceis de interpretar. Além disso, é possível utilizar a macro "doto" para escrever vários valores para o erro padrão em uma única linha de código.

# Veja também:

- [Documentação Clojure para a função pprint](https://clojuredocs.org/clojure.pprint/pprint)
- [Tutorial sobre como lidar com erros em Clojure](https://lispcast.com/clojure-error-handling/)
- [Guia completo de referência para Clojure](https://clojuredocs.org/)