---
date: 2024-01-20 17:30:48.016606-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:37.538458-06:00'
model: gpt-4-1106-preview
summary: .
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

## Hur man gör:
```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

;; Skapa ett datum
(def today (t/now))

;; Beräkna 10 dagar in i framtiden
(def ten-days-later (t/plus today (t/days 10)))

;; Skriv ut dagens datum och datumet 10 dagar senare
(println "Idag är det" (c/to-string today))
(println "Om 10 dagar är det" (c/to-string ten-days-later))

;; Beräkna 5 dagar i det förflutna
(def five-days-ago (t/minus today (t/days 5)))

;; Skriv ut datumet för 5 dagar sedan
(println "För 5 dagar sedan var det" (c/to-string five-days-ago))
```

## Djupdykning
Historiskt sett har datumberäkningar varit viktigt för att spåra händelser över tid. Idag finns det flera bibliotek för att hantera tidsberäkningar i Clojure, som `clj-time`, vilken bygger på Joda-Time biblioteket. Alternativen inkluderar inbyggda Java-bibliotek som `java.time` (Java 8 och framåt). När du implementerar en datumberäkning, tänk på tidszoner och skottsekunder för precision.

## Se även
- Clojure's `clj-time` bibliotek: https://github.com/clj-time/clj-time
- Joda-Time, den inspirationskälla till `clj-time`: https://www.joda.org/joda-time/
- Java 8 Date and Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
