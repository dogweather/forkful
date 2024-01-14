---
title:                "Clojure: Att hämta aktuellt datum"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få den aktuella datumen är en viktig del av många Clojure-program. Det kan hjälpa till att styra logik, registrera händelser och göra tidsberäkningar.

## Hur man gör det
För att få den aktuella datumen i Clojure, använder vi funktionen `java.util.Date` tillsammans med `println`:

```Clojure
(def today (java.util.Date.))
(println today)
```

Detta kommer att skriva ut den aktuella datumen i terminalen:

```
Wed Mar 24 15:12:02 CET 2021
```

Om vi vill ha en mer specifik representation av datumet, som till exempel bara månad och år, kan vi använda funktionen `java.text.SimpleDateFormat` tillsammans med formatsträngen `MMM YYYY`:

```Clojure
(def month-and-year (java.text.SimpleDateFormat. "MMM YYYY"))
(println (month-and-year.format today))
```

Detta kommer att skriva ut:

```
Mar 2021
```

## Djupdykning
`java.util.Date`-klassen har en mängd olika metoder som vi kan använda för att manipulera och hämta olika delar av datumet, som år, månad, dag osv. I Clojure kan vi också använda "threading macro" syntax (`->` och `->>`) för att kedja flera funktioner utan att behöva skriva ut alla argument.

```Clojure
(-> today
    (.getYear)
    (+ 1900)
    (println))
```

Detta kommer att skriva ut året (2021 i detta fall). Med `->>` kan vi göra samma sak fast baklänges, om vi vill. Läs mer om threading macros här: [The Joy of Clojure](https://www.amazon.com/Joy-Clojure-Thinking-Way/dp/1935182641).

## Se även
- [Clojure Cookbook: Dates and Times](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/06_datetimes-and-logical-time.markdown)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)