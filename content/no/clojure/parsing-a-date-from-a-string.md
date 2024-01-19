---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse en dato fra en streng er prosessen med å oversette tekstlig representasjon av en dato til et datatypen som programmeret kan behandle. Vi gjør dette fordi da kan vi manipulere og beregne data, for eksempel å finne forskjellene mellom to datoer.

## Hvordan:
```Clojure 
(require '[java.time :as jt])
(require '[java.time.format :as fmt])

(defn parse-date [date-str] 
  (jt/LocalDate/parse date-str (fmt/DateTimeFormatter/ofPattern "dd-MM-yyyy")))

(println (parse-date "31-12-2020")) 
;; output: 2020-12-31
```
Her tar vi en dato streng "31-12-2020" og konverterer den til LocalDate-format som kan lett blir manipulert i Clojure.

## Dypdykk
1) Historisk kontekst: I de tidlige dagene av programmering, måtte programmererne håndtere dato parsing manuelt. Men moderne språk som Clojure har innebygd støtte for dato parsing.

2) Alternativer: Alternativt til 'java.time', kan du også bruke 'clj-time' bibliotek hvis du bruker en eldre versjon av Clojure eller Joda-Time bibliotek i Java.

3) Implementeringsdetaljer: 'java.time.LocalDate/parse' og 'java.time.format.DateTimeFormatter/ofPattern' er metoder tilgjengelige i Java-biblioteket, og Clojure tillater oss å bruke dem direkte i kode.

## Se Også:
1) Clojure Official Documentation: [https://clojure.org/](https://clojure.org/)
2) Java era and time-manipulation features: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
3) Clojure Cookbook: Date and Time: [https://clojure-cookbook.com/](https://clojure-cookbook.com/)