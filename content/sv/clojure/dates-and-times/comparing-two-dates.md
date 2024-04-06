---
date: 2024-01-20 17:32:35.110405-07:00
description: "Hur man g\xF6r: F\xF6r l\xE4nge sedan, i Java-v\xE4rlden, skapades `java.util.Date`\
  \ f\xF6r att hantera datum och tid. Med tiden visade det sig ha designbrister, som\u2026"
lastmod: '2024-04-05T21:53:38.865814-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r l\xE4nge sedan, i Java-v\xE4rlden, skapades `java.util.Date` f\xF6\
  r att hantera datum och tid."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## Hur man gör:
```Clojure
;; importera java.util.Date
(import java.util.Date)

;; skapar två datum
(def date1 (Date.))
(Thread/sleep 1000) ;; pausar för 1 sekund
(def date2 (Date.))

;; jämför två datum
(defn compare-dates [d1 d2]
  (cond
    (< (.compareTo d1 d2) 0) "date1 är före date2"
    (> (.compareTo d1 d2) 0) "date1 är efter date2"
    :else "datumen är lika"))

;; exempel på användning
(println (compare-dates date1 date2))
;; Output: "date1 är före date2"
```

## Fördjupning
För länge sedan, i Java-världen, skapades `java.util.Date` för att hantera datum och tid. Med tiden visade det sig ha designbrister, som bristande tidszonstöd. Därför introducerades `java.time.*` med Java 8. Clojure, byggd på JVM, kan använda båda.

Förutom `java.util.Date`, kan Clojure-användare använda Joda-Time eller `java-time` biblioteket för ett bättre API. Dessa bibliotek hanterar tidszoner, olikhet i kalendrar och är mer intuitiva med operationer som att lägga till tidsperioder.

Detaljerna i att jämföra två datum kan bli komplexa. Tänk på skottår, olika tidszoner och kalendrar. Viktigt att testa din logik noga för att kringgå subtila fallgropar.

## Se även
- [Clojure java-time](https://github.com/dm3/clojure.java-time) - ett bibliotek för modern tid och datumhantering.
- [Joda-Time](https://www.joda.org/joda-time/) - ett användarvänligt tidshanteringbibliotek.
- [Clojure Docs](https://clojuredocs.org/) - en bra resurs för att utforska Clojure-funktioner och -bibliotek.
