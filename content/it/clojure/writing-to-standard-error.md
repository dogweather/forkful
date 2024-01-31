---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error, STDERR, permette di separare errori e log dagli output normali del programma. Questo è essenziale per il debug e la gestione degli errori.

## How to:
In Clojure, `(binding [*err* *out*] ...)` reindirizza STDERR a STDOUT, e `(binding [*out* *err*] ...)` fa l'opposto. Ecco come usare STDERR:

```Clojure
;; Scrivere a STDERR
(binding [*err* *out*]
  (println "Questo è un messaggio di errore"))

;; Output
Questo è un messaggio di errore
```

## Deep Dive
Prima del Clojure, molte lingue hanno gestito STDERR simile, con meccanismi di reindirizzamento integrati. Come alternativa, library di logging offrono più controllo. Internamente, la scrittura su STDERR in Clojure avviene attraverso Java interop, dato che Clojure è hostato su JVM.

## See Also
- Documentazione Clojure su [binding](https://clojuredocs.org/clojure.core/binding)
- Library di logging Clojure come [Timbre](https://github.com/ptaoussanis/timbre)
- [Java interop in Clojure](https://clojure.org/reference/java_interop)
