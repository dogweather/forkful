---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Clojure: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Beregning av en fremtidig eller tidligere dato innebærer å legge til eller trekke dager, måneder etc. fra en gitt dato. Dette hjelper programmerere med å håndtere tidssensitiv informasjon som utløpsdatoer, minnere, planlegging, og mer.

## Slik gjør du:
Clojure tilbyr Java interop for dato manipulering. Bruk `java.time.LocalDateTime` eller `java.time.LocalDate` for dette:

```Clojure
(require '[java-time :as jt])

(defn days-ahead [day]
  (->> (jt/local-date)
       (jt/plus (jt/days day))))

;; eksempel output
(days-ahead 10)
;; => 2031-12-30

(defn days-before [day]
  (->> (jt/local-date)
       (jt/minus (jt/days day))))

;; eksempel output
(days-before 10)
;; => 2031-12-10
```
Her legger `days-ahead` til dager mens `days-before` trekker dager fra dagens dato.

## Dypdykk
Beregning av datoer er en eldgammel praksis, og i den moderne verden blir den benyttet i mange bruksområder fra minnere til finansielle beregninger.

Alternativt kan du bruke biblioteket "clj-time" for lignende funksjonalitet. Men, siden JDK 8, har "java.time" -biblioteket blitt den foretrukne måten for dato- og tidsmanipulering på grunn av sin rikholdige API og overlegen ytelse.

Implementasjonsdetaljer kan variere basert på bibliotekene som brukes, men de fleste vil tilby metoder for å legge til eller trekke fra tid på samme måte som eksemplet.

## Se Også
For en mer inngående diskusjon, sjekk ut følgende lenker:
- [Java's LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Clojure/java-time](https://github.com/dm3/clojure.java-time)
- [clj-time](https://github.com/clj-time/clj-time)