---
title:                "At få den aktuella datumet."
html_title:           "Clojure: At få den aktuella datumet."
simple_title:         "At få den aktuella datumet."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att få den nuvarande datumen är en vanlig uppgift för programmerare. Det är en enkel och användbar funktion som hjälper till med att spåra tidsstämplar i program och applikationer.

## Hur man gör:
För att få den nuvarande datumen i Clojure, kan vi använda funktionen `(java.time.LocalDate/now)`. Detta kommer att returnera en `LocalDate` objekt som representerar den aktuella datumen. Du kan också specificera en tidszon genom att använda funktionen `(java.time.ZonedDateTime/now)`.

```Clojure
(java.time.LocalDate/now)
=> #object[java.time.LocalDate 0x19be4a12 "2021-08-10"]
 
(java.time.ZonedDateTime/now (java.time.ZoneOffset/UTC))
=> #object[java.time.ZonedDateTime 0x4e1ffda6"2021-08-10T00:00Z[UTC]"]
```

## Djupdykning:
För att förstå hur man får den nuvarande datumen i Clojure, behöver vi förstå lite om datum och tidshantering. Historiskt sett har det funnits många olika sätt att representera datum och tid, men de flesta moderna programmeringsspråk använder ISO-standarden för dag och tidshantering.

Det finns också andra alternativ för att få den nuvarande datumen i Clojure, till exempel genom att använda bibliotek som Joda-Time eller clj-time. Dessa bibliotek erbjuder fler funktioner och mer flexibilitet vid hantering av datum och tid.

För att implementera funktionen `(java.time.LocalDate/now)` i Clojure, använder vi Java Time API som introducerades i Java 8. Detta API ger en robust och standardiserad metod för att hantera datum och tid i Java-baserade språk.

## Se även:
- Java Time API-dokumentation: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Joda-Time biblioteket: https://www.joda.org/joda-time/
- clj-time biblioteket: https://github.com/clj-time/clj-time