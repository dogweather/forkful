---
title:                "Att hämta aktuellt datum"
html_title:           "Clojure: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet är en grundläggande funktion inom många programmeringsprojekt. Det kan användas för att spåra tidsstämplar, schemaläggning eller enkelt visa det aktuella datumet för användaren.

## Hur man gör

```Clojure
(let [idag (java.util.Date.)] ; skapar en variabel 'idag' som lagrar aktuellt datum
  (str "Idag är det " (.getDay idag) "/" (.getMonth idag) "/" (.getYear idag))) ; strängkonkatenering för att skriva ut datumet på ett läsbart sätt
```
Utmatning: Idag är det 19/11/2021

```Clojure
(require '[clojure.java-time :as t]) ; importera funktionerna för tidshantering
(t/unzoned-date-time) ; returnerar ett unzoned datum och tid i ISO-8601 format
```
Utmatning: #object[java.time.LocalDateTime 0x1597263 "2021-11-19T00:00"]

## Djupdykning

För mer specifika behov, såsom att få datum i en viss tidszon eller format, finns det olika bibliotek tillgängliga. En populär är clj-time, som erbjuder mer robusta funktioner för tidshantering.

## Se även

- Clojure Dokumentation: https://clojure.org/
- Java Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Clj-Time bibliotek: https://github.com/clj-time/clj-time