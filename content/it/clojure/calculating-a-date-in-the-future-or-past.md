---
title:                "Clojure: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con dati temporali, può essere necessario calcolare una data in futuro o in passato. Questo può essere utile in vari scenari, come pianificare eventi o gestire scadenze.

## Come fare

Per eseguire questo tipo di calcolo in Clojure, è possibile utilizzare la funzione `clj-time` combinata con la libreria `clojure.java-time`. Iniziamo importando entrambe le librerie e definendo una data di riferimento:

```Clojure
(require '[clj-time.core :as t]
         '[java-time :as jt])

(def today (jt/now))
```

Per calcolare una data in futuro, possiamo utilizzare la funzione `plus` di `clj-time` fornendo il numero di giorni, mesi o anni desiderato come parametro:

```Clojure
(def tomorrow (t/plus today (t/days 1)))
```

Analogamente, per calcolare una data in passato, possiamo utilizzare la funzione `minus` di `clj-time`:

```Clojure
(def yesterday (t/minus today (t/days 1)))
```

Possiamo anche utilizzare gli operatori aritmetici di base per effettuare questi calcoli:

```Clojure
(def next-week (t/plus today (t/weeks 1)))
(def last-week (t/minus today (t/weeks 1)))
```

Infine, per ottenere solo la data senza l'orario, possiamo utilizzare la funzione `get-date` di `clojure.java-time`:

```Clojure
(jt/get-date today) ; => #date "2020-05-20"
```

## Approfondimento

E' importante notare che quando si effettuano calcoli con date in Clojure, il risultato è sempre un oggetto `org.joda.time.DateTime`, che consente di effettuare ulteriori operazioni come confronti o formattazioni.

Se si vuole calcolare una data in base alla data attuale del sistema, si può utilizzare la funzione `jt/zoned-now` di `clojure.java-time` invece di `jt/now`:

```Clojure
(def today (jt/zoned-now))
```

Per ulteriori informazioni su come gestire date e orari in Clojure, si possono consultare le seguenti risorse:

- [Documentazione di clj-time](https://github.com/clj-time/clj-time)
- [Documentazione di clojure.java-time](https://github.com/dm3/clojure.java-time)
- [Articolo sulle date in Clojure di Daniel Higginbotham](https://www.braveclojure.com/core-functions-in-depth/#chronology)
- [Video tutorial sulle date in Clojure di PurelyFunctional.tv](https://purelyfunctional.tv/article/calculating-dates-clojure/)

## Vedi anche

- [Funzioni per la gestione di date in Clojure](https://clojuredocs.org/clojure.java-time/local-date)
- [Esempi di calcolo di date in Clojure](https://gist.github.com/nsklaus/6112660)
- [Calcolo di date utilizzando la libreria clj-time](https://poundbang.in/software/life/2017/02/01/tools-of-the-clojure-trade-clj-time.html)