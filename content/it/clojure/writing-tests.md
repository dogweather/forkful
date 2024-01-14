---
title:    "Clojure: Scrivere test"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Perché
Scrivere test è un passo importante nel processo di sviluppo di un progetto in Clojure. I test ci permettono di verificare che il nostro codice funzioni correttamente, evitando bug e facilitando il processo di debugging.

## Come Fare
Per scrivere test in Clojure, utilizzeremo una libreria chiamata "clojure.test". Iniziamo importandola nel nostro file:

```Clojure
(ns my-project.test
  (:require [clojure.test :refer :all]))
```

Ora possiamo definire una funzione di test utilizzando la macro "deftest". Darle un nome che descriva il comportamento che vogliamo testare:

```Clojure
(deftest test-addition
  (is (= (+ 1 2) 3))) ;; Il test deve fallire perché 1+2=3
```

Infine, possiamo eseguire il nostro test utilizzando la funzione "run-tests":

```Clojure
(run-tests)
```

Se il test fallisce, verrà mostrato un messaggio di errore con una descrizione del problema. Altrimenti, verrà mostrato un messaggio di successo.

## Approfondimento
Scrivere test efficaci richiede un po' di pratica e conoscenza delle diverse funzioni e macro disponibili in "clojure.test". Alcune delle più utilizzate sono:

- "is": utilizzata per confrontare valori e determinare se il test è passato o fallito
- "testing": permette di organizzare i test in gruppi e specificare le condizioni di esecuzione
- "testing-throw": utile per testare se una specifica funzione solleva un'eccezione
- "are": permette di testare più di una condizione nello stesso test

È importante anche comprendere il concetto di "mocking" nel testing, che ci permette di simulare determinati comportamenti e testare casi più complessi.

## Vedi Anche
- [Documentazione di clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Clojure Cookbook: Writing Tests](https://clojure-cookbook.timothypratley.com/pages/testing.html)
- [The Art of Prograniming in Clojure: Chapter 24 - Testing](https://www.amazon.com/Art-Programming-Clojure-Edition/ebook/dp/B01LWCDUGU/ref=sr_1_1?keywords=the+art+of+programming+in+clojure&qid=1560053044&s=gateway&sr=8-1)