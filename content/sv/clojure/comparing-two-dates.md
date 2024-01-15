---
title:                "Jämförelse av två datum"
html_title:           "Clojure: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart i många olika situationer, som att kontrollera om ett evenemang redan har skett, sortera data efter datum eller beräkna hur lång tid det har gått sedan en händelse.

## Hur man gör
Att jämföra två datum i Clojure är enkelt med hjälp av inbyggda funktioner som `time/now` och `time/instant`. Det är viktigt att notera att datumen behöver vara i rätt format för att jämförelsen ska fungera som förväntat.

Ett exempel på hur man jämför två datum och bestämmer vilket som är tidigare:
```Clojure
(def datum1 (time/instant)) ; Skapar ett datum med aktuellt datum och tid
(def datum2 (time/instant "2019-02-01T12:00:00")) ; Skapar ett datum med ett specificerat datum och tid

(if (< datum1 datum2)
  (println "datum1 är tidigare än datum2")
  (println "datum2 är tidigare än datum1"))
```

Output:
```
datum2 är tidigare än datum1
```

## Djupdykning
När man jämför två datum i Clojure är det viktigt att förstå hur datumen faktiskt representeras i programmet. I Clojure används den inbyggda datatypen `java.time.Instant` för att hantera datum och tid. Datumen representeras som en tidpunkt i millisekunder sedan 1970-01-01 00:00 UTC. Detta kallas också för "epoch" eller "Unix-time".

En viktig sak att tänka på är att de inbyggda funktionerna `time/now` och `time/instant` returnerar datumen i UTC-tid och inte lokaltid. Detta kan leda till felaktiga jämförelser om man inte tar hänsyn till tidszoner.

## Se även
- [Clojure Time Library](https://github.com/clj-time/clj-time)
- [Instant documentation](https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html)