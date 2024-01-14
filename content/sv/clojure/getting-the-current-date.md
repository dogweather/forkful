---
title:                "Clojure: Få den aktuella datumen"
simple_title:         "Få den aktuella datumen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas flera anledningar till varför man vill få den aktuella datumet i ett Clojure-program. Det kan vara för att använda det som en tidsstämpel, visa tidsbegränsad information eller helt enkelt för att förbättra användarupplevelsen.

## Hur man gör

Först behöver vi importera Clojure built-in biblioteket "java. util. Calendar" för att kunna använda de inbyggda funktionerna för datum och tid. Sedan kan vi använda följande kod för att hämta den aktuella datumen:

```Clojure
(def currentDate (java.util.Calendar/getInstance))
```

För att få ut datumet i ett visst format kan vi använda funktionen "get" och ange önskad tidskomponent som argument. Till exempel, om vi vill få datumet i formatet "yyyy-MM-dd" (år-månad-dag) kan vi använda följande kod:

```Clojure
(.get currentDate java.util.Calendar/DATE)
```

Detta kommer att returnera en integer som representerar dagens datum. Vi kan sedan använda Clojure-funktionen "format" för att formatera det till önskat format. Här är ett exempel på hur det kan se ut:

```Clojure
(def desiredFormat "yyyy-MM-dd")
(format "%1$td-%1$tm-%1$tY" (.getTime currentDate))
```

Detta kommer att returnera en sträng med "dag-månad-år" formatet. Det finns många andra inbyggda funktioner och databibliotek som kan användas för att manipulera datum och tid i Clojure.

## Djupdykning

För de som är intresserade av mer avancerade sätt att hantera datum och tid i Clojure, finns det olika tredjepartsbibliotek såsom "tawny-owl" och "java.time" som erbjuder mer avancerade funktioner och förenklar hanteringen av datum och tid ännu mer. 

## Se även

- [Clojure Standard Library](https://clojuredocs.org/quickref)
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tawny-owl Library](https://github.com/flatland/tawny-owl)