---
title:                "Clojure: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Har du någonsin behövt jämföra två datum i ditt Clojure-program? Det finns flera anledningar till varför man kanske skulle vilja göra detta, till exempel för att avgöra vilket datum som är senare eller för att kontrollera om ett datum ligger inom en viss tidsram.

## Hur man gör det
Att jämföra två datum i Clojure är enkelt med hjälp av inbyggda funktioner som `compare` och `before?/after?`. Här är ett exempel på hur man kan jämföra två datum:

```
(def date1 (java.util.Date. 2021 10 1))
(def date2 (java.util.Date. 2021 12 1))

```Clojure
;; Jämför date1 med date2
(compare date1 date2)

;; Output: -1 (date1 är tidigare än date2)

;; Kontrollera om date1 kommer före date2
(before? date1 date2)

;; Output: true
```

## Djupdykning
När man jämför två datum är det viktigt att vara medveten om hur Clojure behandlar dem. Datum i Clojure representeras som antal millisekunder från Unix-epoken (1 januari 1970). Detta innebär att när du jämför två datum, så jämför du egentligen antalet millisekunder mellan dem.

En viktig sak att komma ihåg är att ju senare datumet är, desto större är antalet millisekunder och därför kommer den att vara "större" än ett tidigare datum när man använder `compare` eller `before?/after?`.

## Se även
Här är några länkar med mer information om att jämföra datum i Clojure:

* [Officiell dokumentation om `compare`](https://clojuredocs.org/clojure.core/compare)
* [Komparera datum med Joda-Time library](https://www.thoughtworks.com/insights/blog/clojure-dynamic-comparing-date-time)
* [Mer information om datumrepresentation i Clojure](https://clojure.org/reference/chronology)