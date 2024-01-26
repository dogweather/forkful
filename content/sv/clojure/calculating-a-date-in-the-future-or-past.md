---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:30:48.016606-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett datum i framtiden eller förflutna är precis det det låter som - att hitta ett datum före eller efter ett känt datum. Programmerare gör detta för att hantera tidsbaserade uppgifter som att skapa påminnelser, generera rapporter, eller för att hålla koll på tidsperioder för avtal.

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
