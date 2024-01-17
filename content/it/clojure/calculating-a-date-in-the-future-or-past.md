---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Clojure: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Calcolare una data in futuro o in passato è un'operazione comune nella programmazione, e si riferisce al calcolo di una data basandosi su una data di partenza e un intervallo di tempo specificato. I programmatori lo fanno per una varietà di motivi, tra cui la pianificazione di eventi, la gestione di scadenze e il calcolo di valori temporali in un'applicazione.

## Come:
```Clojure
(require '[clj-time.core :as time])
(time/plus (time/now) (time/day-of 2)) ; calcola la data di oggi più due giorni
=> #object[org.joda.time.DateTime 0x649218ad "2020-10-30T19:26:17.123Z"]
(time/minus (time/now) (time/hour-of 3)) ; calcola la data di oggi meno tre ore
=> #object[org.joda.time.DateTime 0x63817ca6 "2020-10-28T19:26:17.123Z"]
```

## Deep Dive:
Il calcolo di una data in futuro o in passato ha radici storiche nell'uso dei calendari per tenere traccia del tempo. Ci sono diversi modi per farlo, tra cui l'uso di librerie come "clj-time" o la creazione di funzioni personalizzate utilizzando le funzionalità di date fornite dal linguaggio. Inoltre, ci sono varie considerazioni da tenere a mente durante l'implementazione, come l'uso di fusi orari e la gestione di date in formato UTC.

## Vedi anche:
- [Documentazione clj-time](https://github.com/clj-time/clj-time)
- [Tutorial su come lavorare con le date in Clojure](https://practicalli.github.io/clojure/dates-and-time.html)
- [Esplorazione dei fusi orari in Clojure](https://purelyfunctional.tv/guide/time-in-clojure/timezones/)