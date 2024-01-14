---
title:                "Clojure: Stampa dell'output di debug"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si sta scrivendo codice in Clojure, si possono incontrare alcuni problemi o bug difficili da risolvere. In tali casi, può essere utile stampare gli output di debug per capire il flusso del programma e individuare eventuali errori.

## Come fare

Per stampare l'output di debug in Clojure, possiamo utilizzare la funzione `println`, che stampa una stringa sullo standard output. Possiamo anche usare la macro `spy` della libreria `clojure.inspector` per stampare il valore di una variabile in un certo punto del nostro codice. Vediamo un esempio pratico:

```Clojure
(defn add [x y]
  (println "Input: " x y)
  (+ x y))

(let [a 3
      b 5]
  (println "Result: " (add a b)))

;; Output:
;; Input: 3 5
;; Result: 8
```

In questo esempio, abbiamo definito una semplice funzione `add` che stampa gli input ricevuti e restituisce la somma dei due numeri. Nella seconda parte del codice, abbiamo assegnato dei valori alle variabili `a` e `b` e stampato il risultato della funzione `add`. Vediamo come l'output di debug ci aiuta a capire il funzionamento del nostro codice e a verificare i valori delle variabili.

## Approfondimento

E se vogliamo avere maggiori informazioni sull'output di debug? Possiamo utilizzare la funzione `pprint` per stampare una rappresentazione leggibile dei dati su più righe. Inoltre, possiamo anche utilizzare la macro `prn` per stampare valori in modo più strutturato, separando gli elementi con uno spazio e andando a capo alla fine.

Ecco un esempio pratico dell'utilizzo di `pprint` e `prn`:

```Clojure
(defn check [x]
  (if (> x 10)
    (println "Il valore è maggiore di 10")
    (println "Il valore è minore o uguale a 10")))

(pprint (range 10))
(prn (check 5))

;; Output:
;; (0 1 2 3 4 5 6 7 8 9)
;; Il valore è minore o uguale a 10
```

In questo esempio, la funzione `check` controlla se il valore passato è maggiore di 10 e stampa un messaggio di conseguenza. Abbiamo anche utilizzato `pprint` per stampare il range dei numeri da 0 a 9 e `prn` per stampare il risultato della funzione `check`.

## Vedi anche

- [Documentazione di Clojure][1]
- [Guida alla risoluzione dei bug in Clojure][2]

[1]: https://clojure.org/
[2]: https://purelyfunctional.tv/article/debugging-in-clojure/