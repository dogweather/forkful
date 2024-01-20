---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum innebär att avgöra vilket datum som kommer först eller senast. Programmerare gör detta för att söka, sortera, filtrera data baserat på datum.

# Hur man:

Här är ett exempel på hur man jämför två datum i Clojure:

```Clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c])

(defn compare-dates [date1 date2]
(let [d1 (c/from-date date1)
      d2 (c/from-date date2)]
  (t/compare d1 d2)))
```

Här är ett prov på hur det fungerar:

```Clojure
(let [date1 (t/date-time 2021 12 1)
      date2 (t/date-time 2022 1 1)]
  (compare-dates date1 date2))
```

Produktionen kommer att vara `-1` som visar att det första datumet är tidigare än det andra.

## Djupdykning

Historiskt sett skapades inbyggda datum- och tidsfunktioner i de flesta programmeringsspråk för att underlätta datumhantering. Men dessa funktioner fungerar något annorlunda i varje språk. I Clojure tillhandahåller clj-time-biblioteket en tidsram för datum- och tidshantering. Den är konstruerad över Jodatid, som är en förbättring över Javas ursprungliga datum / tidklasser.

Ett alternativ för att jämföra datum är att omvandla datum till tidsstämplar och jämföra dem. Men detta kan leda till oväntade resultat om tidzoner inte hanteras korrekt.

Den underliggande implementeringen av jämföringsfunktionen i clj-time använder Long's compareTo-metod, som ger ett negativt tal, noll eller ett positivt tal beroende på om det första värdet är mindre, lika med eller större än det andra.

## Se Också

För mer information om hur du hanterar datum och tid i Clojure, se dessa resurser:

1. [clj-time Github Repository](https://github.com/clj-time/clj-time)
2. [Joda-Time Library](http://www.joda.org/joda-time/)
3. [Clojure Documentation](https://clojure.org/)
4. [Java 8 Date / Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)