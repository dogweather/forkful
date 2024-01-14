---
title:                "Clojure: Omvandla ett datum till en sträng"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara användbart när man vill visa datumet i ett specifikt format eller spara det som en sträng i en databas. Det är också ett vanligt problem som man kan stöta på i programmering och det är därför viktigt att veta hur man hanterar det på ett effektivt sätt.

## Hur man gör det

```Clojure
(require '[clojure.java-time :as time])
(import '[java.time.format DateTimeFormatter])

;; Skapa ett datumobjekt
(def date (java.time.LocalDate/now))

;; Konvertera till en sträng med standardformatet
(time/format date)
;; => "2021-10-12"

;; Använda ett anpassat format
(time/format date "dd.MM.yyyy")
;; => "12.10.2021"

;; Ställa in olika format
(def custom-formatter (DateTimeFormatter/ofPattern "yyyy/MM/dd"))
(time/format date custom-formatter)
;; => "2021/10/12"

```

Det finns många olika formatteringsalternativ beroende på vilket datum- och tidspaket som används. Det är alltid en god idé att läsa dokumentationen eller använda en formatteringsguide för att hitta det mest lämpliga alternativet för ditt specifika behov.

## Djupdykning

När man konverterar ett datum till en sträng bör man tänka på några viktiga aspekter. Till exempel kan olika tidszoner påverka hur datumet visas eller om det är ett lokaliserat eller standardiserat datumformat. Det finns också möjlighet att formatera och jämföra datum på ett mer unikt sätt med hjälp av Clojure's chrono bibliotek.

## Se även

- [Clojure's java-time bibliotek](https://clojure.github.io/java-time/)
- [Formatteringsguiden för Java's standardiserade format](https://docs.oracle.com/javase/tutorial/i18n/format/sdf.html)
- [Chrono biblioteket för Clojure](https://github.com/clj-time/chronoj)