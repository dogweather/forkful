---
title:                "Confronto di due date"
html_title:           "Clojure: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori si trovano spesso ad affrontare il problema di dover confrontare due date in applicazioni Clojure. Questo è un elemento fondamentale nell'elaborazione dei dati e può portare a complicazioni se non affrontato correttamente. In questo articolo, vedremo come confrontare due date in modo efficace utilizzando Clojure.

## Come fare

Per confrontare due date in Clojure, possiamo utilizzare la funzione `compare` del core Clojure. Questa funzione restituisce un valore intero che rappresenta la differenza tra le due date. Vediamo un esempio pratico:

```Clojure
(def d1 (java.util.Date. 2019 01 01))
(def d2 (java.util.Date. 2020 01 01))

(def result (compare d1 d2))

(println result) ; Output: -1
```

In questo esempio, abbiamo creato due istanze della classe `java.util.Date`, una per il 1° gennaio 2019 e una per il 1° gennaio 2020. Poi, abbiamo utilizzato la funzione `compare` per confrontarle e abbiamo salvato il risultato in una variabile `result`. Come possiamo vedere dall'output, il risultato è -1, il che significa che la prima data (d1) è prima della seconda data (d2).

Ma cosa succede se confrontiamo due date che sono uguali?

```Clojure
(def d1 (java.util.Date. 2020 01 01))
(def d2 (java.util.Date. 2020 01 01))

(def result (compare d1 d2))

(println result) ; Output: 0
```

In questo caso, il risultato sarà 0, indicando che le due date sono uguali. E se invece vogliamo confrontare le date in base all'ordine cronologico e non all'ordine alfabetico?

```Clojure
(def d1 (java.util.Date. 2020 01 01))
(def d2 (java.util.Date. 2020 02 01))

(def result (compare d1 d2))

(println result) ; Output: -1
```

Per risolvere questo problema possiamo utilizzare la funzione `compare-dates` del modulo `clojure.java-time`, che confronta le date in base all'ordine cronologico. Vediamo un ultimo esempio:

```Clojure
(require '[java-time :refer [compare-dates]])

(def d1 (java.util.Date. 2020 03 01))
(def d2 (java.util.Date. 2020 01 01))

(def result (compare-dates d1 d2))

(println result) ; Output: 1
```

In questo caso, il valore del risultato è 1, indicando che la prima data (d1) viene dopo la seconda data (d2). Possiamo anche utilizzare questa funzione per confrontare date locali, ad esempio:

```Clojure
(def d1 (java-time/local-date 2020 03 01 0 0 0))
(def d2 (java-time/local-date 2020 01 01 0 0 0))

(def result (compare-dates d1 d2))

(println result) ; Output: 1
```

## Deep Dive

La funzione `compare` del core Clojure si basa sulla classe `java.util.Date`, quindi non è consigliata per confrontare date locali. Inoltre, la funzione `compare` utilizza il sistema di confronto predefinito, che può variare a seconda del sistema operativo e delle impostazioni locali. È importante quindi utilizzare la funzione `compare-dates` del modulo `clojure.java-time` per confrontare date in modo robusto e consistente.

## Vedi anche

- [Documentazione Clojure su compare e compare-dates](https://clojure.github.io/clojure/branch-master/clojure.core-api.html#clojure.core/compare)
- [Java-Time Clojure library](https://github.com/java-time/java-time)
- [Tutorial di Clojure](https://www.clojure.org/guides/learn/syntax)