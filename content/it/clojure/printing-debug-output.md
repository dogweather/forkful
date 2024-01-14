---
title:                "Clojure: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug è una pratica comune per risolvere problemi e ottenere una migliore comprensione del codice. Con il linguaggio di programmazione Clojure, è possibile utilizzare vari strumenti per generare output di debug utili e informativi.

## Come

Per stampare output di debug in Clojure, puoi utilizzare la funzione `println` e passare come argomento la variabile o l'espressione che vuoi stampare. Ad esempio:

```Clojure
(def nome "Mario")
(println nome)
```

Questo stamperebbe "Mario" nella console come output di debug.

Puoi anche utilizzare la macro `prn` per stampare una rappresentazione pratica di un'intera struttura di dati. Ad esempio:

```Clojure
(def numeri [1 2 3])
(prn numeri)
```

Questo stamperebbe il vettore [1 2 3] nella console come output di debug.

Puoi anche combinare `println` e `prn` per stampare output più dettagliati, ad esempio:

```Clojure
(def x 5)
(def y 10)
(println "Il valore di x è:" x)
(prn "Il valore di y è:" y)
```

Questo stamperebbe nella console "Il valore di x è: 5" seguito da "Il valore di y è: 10".

## Approfondimento

Oltre alle funzioni di base `println` e `prn`, Clojure offre anche il logging tramite la libreria `clojure.tools.logging`. Questa libreria permette di stampare output di debug con diversi livelli di severità (ad esempio `info`, `debug`, `warn`), rendendo più semplice comprendere e organizzare gli output.

Un altro strumento utile per il debugging in Clojure è il REPL (Read-Eval-Print Loop). Questo ambiente interattivo permette di eseguire istruzioni in modo rapido e controllare i risultati. Utilizzando il REPL, è possibile inserire output di debug direttamente nel codice e vedere istantaneamente i risultati.

## Vedi anche

- [Documentazione Clojure "Debugging"](https://clojure.org/guides/debugging)
- [Clojure Logging - Clojure Cookbook](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/08_logging.asciidoc)
- [REPL - Clojure Docs](https://clojure.org/reference/repl)