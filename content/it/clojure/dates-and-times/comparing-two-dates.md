---
date: 2024-01-20 17:32:48.829064-07:00
description: "Come Fare: Comparare date \xE8 un'esigenza comune fin dall'inizio dell'era\
  \ informatica. Prima di Clojure, i linguaggi come C++ e Java avevano le loro\u2026"
lastmod: '2024-04-05T21:53:43.836490-06:00'
model: gpt-4-1106-preview
summary: "Comparare date \xE8 un'esigenza comune fin dall'inizio dell'era informatica."
title: Confronto tra due date
weight: 27
---

## Come Fare:
```Clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.format :as format])

; Creare due date
(def date1 (time/date-time 2021 3 15))
(def date2 (time/date-time 2023 1 10))

; Confrontare date
(time/before? date1 date2) ; => true
(time/after? date1 date2) ; => false
(time/equal? date1 date2) ; => false

; Differenza in giorni
(def days-between (time/in-days (time/interval date1 date2)))
; => 666

; Formattare e confrontare date come stringhe
(def fmt (format/formatters :basic-date))
(def str-date1 (format/unparse fmt date1))
(def str-date2 (format/unparse fmt date2))
(= str-date1 str-date2) ; => false
```

## Approfondimento
Comparare date è un'esigenza comune fin dall'inizio dell'era informatica. Prima di Clojure, i linguaggi come C++ e Java avevano le loro librerie per gestire le date. In Clojure, clj-time, basata sulla libreria Joda-Time, permette la manipolazione e il confronto di date. Alternativamente, possiamo usare la `java.time` API disponibile in Java 8+. clj-time offre un'API coerente e funzionale, adattandosi ai paradigmi di Clojure.

## Vedi anche
- clj-time GitHub: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Clojure official documentation: [https://clojure.org/](https://clojure.org/)
- `java.time` guide: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
