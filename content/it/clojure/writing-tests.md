---
title:                "Clojure: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è un'attività fondamentale nella programmazione che aiuta a verificare il corretto funzionamento del codice. Non solo garantisce un comportamento affidabile dell'applicazione, ma inoltre semplifica il processo di debugging.

## Come fare

Per scrivere test in Clojure sono necessari due strumenti fondamentali: `clojure.test` e `clojure.spec`. Il primo è una libreria standard di Clojure che offre un'interfaccia intuitiva per la creazione di test, mentre il secondo viene usato per definire le specifiche di ogni funzione.

Per utilizzare `clojure.test`, è necessario definire una funzione che contiene tutti i test, utilizzando il marcatore `deftest`. All'interno di questa funzione, è possibile utilizzare il marcatore `is` per verificare i valori restituiti dalle funzioni testate.

```Clojure
(deftest test-somma
  (is (= 4 (+ 2 2)))
  (is-not (= 5 (+ 2 2))))
```

Per quanto riguarda `clojure.spec`, invece, è possibile definire le specifiche delle funzioni utilizzando il marcatore `s/fdef` e specificando il nome, gli argomenti e il valore di ritorno della funzione. Questo permette di verificare automaticamente che la funzione rispetti le specifiche in fase di test.

```Clojure
(s/fdef somma :args int? int? :ret int?)
(defn somma [a b]
  (+ a b))
```

Una volta definite le specifiche, è possibile utilizzare la funzione `s/check` per controllare se una data funzione ne rispetta i requisiti.

## Approfondimento

Scrivere test è un processo importante e bisogna considerare alcune best practices per ottenere i migliori risultati. È importante scrivere test che siano indipendenti e riproducibili, in modo da poterli eseguire più volte senza influire sul risultato. Inoltre, è consigliato scrivere test per ogni possibile scenario, in modo da coprire tutti i casi.

È anche fondamentale capire quale parte del codice va testata e quale no. Non tutte le parti del nostro programma necessitano di un test specifico e quindi è importante effettuare una scelta consapevole per evitare test ridondanti o poco utili.

## Vedi anche

- [Documentazione ufficiale di clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Documentazione ufficiale di clojure.spec](https://clojure.org/guides/spec)