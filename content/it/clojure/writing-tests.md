---
title:                "Scrivere test"
html_title:           "Clojure: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Scrivere test è un modo per verificare che il nostro codice funzioni correttamente. I programmatori lo fanno per garantire che il loro codice sia affidabile e che funzioni correttamente, prima di inviarlo in produzione.

## Come fare:

Scrivere test in Clojure è facile! Basta usare la libreria di test incorporata nel linguaggio, chiamata ```clojure.test```. Di seguito un esempio di un test semplice che verifica che la funzione ```add``` sommi correttamente due numeri:

```Clojure
(ns test-example
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest add-test
  (testing "Addition Test"
    (is (= (add 1 2) 3))
    (is (= (add -1 5) 4))))
```

Esaminiamo questo codice da vicino:

- La prima riga definisce il namespace per il nostro file di test, ```test-example```. Ogni file di test dovrebbe avere un namespace unico.

- La seconda riga importa la libreria di test e il namespace della nostra applicazione, ```example.core```, che contiene la funzione ```add```.

- La terza riga definisce il test stesso, chiamato ```add-test```.

- La quarta riga utilizza la funzione ```testing``` per raggruppare tutti i test per la funzione ```add```.

- Le ultime due righe utilizzano la funzione ```is```, che verifica se il risultato dell'espressione fornita è uguale a quello atteso.

Per eseguire questo test, basta invocare ```lein test``` sulla riga di comando.

## Approfondimento:

Scrivere test è un'attività fondamentale nella pratica della programmazione test-driven development (TDD). Ci sono anche altre librerie di test disponibili per Clojure, come ```midje``` e ```speclj```. In generale, è importante scrivere test che siano facili da mantenere e che coprano tutti i casi possibili del nostro codice.

## Vedi anche:



- [Documentazione di Leiningen](https://leiningen.org/), il gestore di progetto consigliato per sviluppare applicazioni in Clojure.