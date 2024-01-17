---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Clojure: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden handler om å bruke datofunksjoner til å manipulere og presentere datoer på en bestemt måte. Dette er nyttig for programmerere fordi det gjør det enklere å håndtere datoer og koordinere tidsbaserte operasjoner.

## Hvordan:
```Clojure
(def dato (java.util.Date.))
(println "Dagens dato er: " dato)
```

Output: Dagens dato er: Wed Jul 14 13:07:19 BST 2021
```Clojure
(def dato (java.util.Date. 1220222400000))
(format "Fremtidig dato er %1$td-%1$tm-%1$tY" dato)
```

Output: Fremtidig dato er 01-09-2008

## Dypdykk:
Beregning av datoer kan være komplekst, spesielt når det gjelder kalenderjusteringer og tidszoner. Et alternativ til å bruke Java-functions (som vist i kodeblokkene over) kan være å bruke biblioteker som "clj-time" eller "java-time". For å beregne en dato i et gitt antall dager frem i tid kan man også bruke funksjoner som "days-to-date" eller "+ 30 days". Det er også viktig å være klar over eventuelle avvik i beregninger, som for eksempel skuddår, som kan påvirke nøyaktigheten til datoene.

## Se også:
- https://github.com/clj-time/clj-time
- https://github.com/dm3/clojure.java-time
- https://clojuredocs.org/clojure.core/+%23days
- https://dzone.com/articles/calculating-dates-with-clojure