---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e Perchè?
Creare un file temporaneo significa generare un file per un utilizzo ad hoc, che viene generalmente cancellato una volta terminato l'uso. I programmatori lo fanno per vari motivi, tra cui il salvataggio di dati temporanei e le operazioni di debug.

## Come fare:
Ecco un esempio di come creare un file temporaneo in Clojure.

```Clojure
(require '[clojure.java.io :as io])

(defn create-temp-file []
(let [temp-file (java.io.File/createTempFile "prefix" ".suffix")]
  (println (.getPath temp-file))
  (.deleteOnExit temp-file)))

(create-temp-file)
```

In questo codice, la funzione `create-temp-file` crea un file temporaneo e stampa il suo percorso. L'istruzione `.deleteOnExit` assicura che il file venga cancellato una volta che la JVM termina.

Output previsto:
```Clojure
/tmp/prefix12345.suffix
```

## Approfondimento
Creare file temporanei come abbiamo fatto qui, è un'operazione che viene usata da molto tempo nella programmazione. Originariamente, era spesso usato per preservare memoria durante elaborazioni di grandi quantità di dati.

Tuttavia, ci sono alternative. Per esempio, potresti usare la memoria per memorizzare i dati temporanei, se non sono troppo grandi. Così come puoi utilizzare database in memoria come Redis.

Nell'implementazione dettagliata del nostro esempio, abbiamo usato la funzione `java.io.File/createTempFile`. Questa genera un file con un nome unico nella directory temporanea predefinita.

## Vedi Anche
- Documentazione sul modulo `java.io.File` di Java: https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/io/File.html 
- Guida di Clojure su "java.jvm.io": https://clojure.github.io/clojure/clojure.java.io-api.html 
- Guida alla biblioteca Redis: https://redis.io/commands