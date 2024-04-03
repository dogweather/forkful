---
date: 2024-01-20 17:31:07.538509-07:00
description: "Calcolare una data futura o passata significa semplicemente determinare\
  \ quale giorno sar\xE0 o \xE8 stato dopo o prima di un certo lasso di tempo. I\u2026"
lastmod: '2024-03-13T22:44:43.055714-06:00'
model: gpt-4-1106-preview
summary: "Calcolare una data futura o passata significa semplicemente determinare\
  \ quale giorno sar\xE0 o \xE8 stato dopo o prima di un certo lasso di tempo."
title: Calcolo di una data futura o passata
weight: 26
---

## Cos'è e perché?
Calcolare una data futura o passata significa semplicemente determinare quale giorno sarà o è stato dopo o prima di un certo lasso di tempo. I programmatori lo fanno per gestire eventi, scadenze, ricorrenze e per pianificare attività.

## Come fare:
Utilizziamo la libreria clj-time, basata su Joda-Time, per manipolare e calcolare date in Clojure. Installala aggiungendo `[clj-time "0.15.2"]` al tuo file project.clj.

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

;; Calcolare una data 10 giorni nel futuro
(let [oggi (t/now)
      future-date (t/plus oggi (t/days 10))]
  (str "Data futura: " (c/to-string future-date)))

;; Calcolare una data 5 anni nel passato
(let [oggi (t/now)
      past-date (t/minus oggi (t/years 5))]
  (str "Data passata: " (c/to-string past-date)))
```
Esempio di output:
```
"Data futura: 2023-10-30T21:18:47.332Z"
"Data passata: 2018-03-26T21:18:47.332Z"
```

## Focus:
Dai tempi antichi, l'uomo ha creato calendari per tracciare il tempo. In informatica, manipolare le date è sempre stata una sfida per via delle diverse rappresentazioni e fusi orari.

Clj-time fornisce una facciata Clojure per la più robusta libreria Java Joda-Time. Joda-Time è spesso preferita alla `java.util.Date` per la sua immutabilità e l'API fluente.

Mentre clj-time segue il metodo immutabile di Joda-Time, un'alternativa moderna è la API Java `java.time` (JSR-310), introdotta in Java 8 per superare le limitazioni precedenti, non ancora nativamente supportata in Clojure ma utilizzabile con interop.

Accanto a queste scelte, ci sono librerie per parsing, formattazione e aritmetica di date, come `clj-time.format`. E' importante gestire fusi orari e Daylight Saving Time, che possono incidere sui calcoli delle date.

## Vedere anche:
- [clj-time GitHub repo](https://github.com/clj-time/clj-time)
- [Joda-Time](http://www.joda.org/joda-time/)
- [Guida della clojure.java-time](https://github.com/dm3/clojure.java-time)
- [Documentazione ufficiale di Clojure](https://clojure.org/api/api)
