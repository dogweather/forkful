---
title:    "Clojure: Jämförelse av två datum"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Varför
I många programmeringsprojekt krävs det att man jämför två datum för att utföra olika funktioner, till exempel att sortera data eller beräkna tidsintervall. I denna blogginlägg undersöker vi hur man kan jämföra två datum i Clojure.

## Hur man jämför två datum
För att jämföra två datum i Clojure finns det flera sätt att gå tillväga. Ett enkelt sätt är att använda funktionen `compare`, som tar två datum som argument och returnerar ett heltal som indikerar hur de två datumen förhåller sig till varandra. Om det första datumet är före det andra returneras ett negativt tal, om det andra datumet är före returneras ett positivt tal, och om datumen är lika returneras noll.

```Clojure
(compare (local-date "2021-01-15") (local-date "2021-01-20"))

; Output: -1
```

En annan funktion som kan användas för att jämföra datum är `before?` och `after?`. Dessa funktioner returnerar true eller false beroende på om det första datumet är före eller efter det andra.

```Clojure
(after? (local-date "2021-03-10") (local-date "2021-01-20"))

; Output: true
```

`between?` är en annan användbar funktion som kan användas för att kontrollera om ett datum ligger mellan två andra datum.

```Clojure
(between? (local-date "2021-02-05") (local-date "2021-01-01") (local-date "2021-03-10"))

; Output: true
```

## Djupdykning
När man jämför datum är det viktigt att vara medveten om hur olika datumtyper hanteras i Clojure. Till exempel kan man inte direkt jämföra ett datum av typen `local-date` med ett datum av typen `zoned-date-time`. Man måste då konvertera datumen till samma typ, antingen genom att använda funktionen `to-local-date` eller `to-zoned-date-time`.

Det är också viktigt att vara medveten om eventuella tidszoner och att små skillnader i tiden kan påverka resultatet av jämförelser mellan datumen.

## Se även
- [Jämföra datum i Clojure Dokumentation](https://clojure.org/api/java.time)
- [Java Date and Time API Dokumentation](https://docs.oracle.com/javase/tutorial/datetime/iso/overview.html)
- [Clojure Programming Language Dokumentation](https://clojure.org/)