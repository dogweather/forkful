---
title:                "Clojure: Scrivere test"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in Clojure

Scrivere test è un processo cruciale per garantire che il nostro codice funzioni correttamente e rimanga robusto nel tempo. Inoltre, i test aiutano a identificare eventuali bug e a fornire una documentazione vivente per il nostro codice.

## Come scrivere test in Clojure

Per scrivere test in Clojure, dobbiamo prima importare la libreria `clojure.test`. Successivamente, possiamo definire una funzione test utilizzando la macro `deftest` e specificando il suo nome e il suo corpo all'interno di una parentesi. In questo corpo, possiamo fare degli assert per verificare che il nostro codice restituisca i risultati attesi.

```Clojure
(ns test.esempio
  (:use clojure.test))

(deftest test-aggiungi
  (is (= (+ 1 2) 3)))
```

Una volta creato il nostro test, possiamo eseguirlo utilizzando la funzione `run-tests` e passandogli il namespace in cui si trova il nostro test.

```Clojure
(run-tests 'test.esempio)
```

Questo esempio dovrebbe restituire un output simile a questo:

```
Testing test.esempio

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Approfondimenti sui test in Clojure

In Clojure, possiamo utilizzare anche la macro `is` per creare degli assert più dettagliati e specificare messaggi di errore personalizzati. Inoltre, possiamo utilizzare la funzione `are` per testare più casi di input per una stessa funzione.

Oltre ai test di unità, possiamo scrivere anche test di integrazione per verificare il comportamento di diverse parti del nostro codice in un contesto reale. Questi tipi di test possono essere eseguiti utilizzando la libreria `lein-midje` o `lein-test-refresh` per un continuo rinnovo dei test mentre sviluppiamo.

## Vedi anche

- [Documentazione ufficiale di clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Esempi di test in Clojure](https://github.com/clojure/test.check/tree/master/examples)
- [Libreria lein-midje per i test di integrazione](https://github.com/marick/Midje) 
- [Libreria lein-test-refresh per il continuo rinnovo dei test](https://github.com/jakemcc/lein-test-refresh)