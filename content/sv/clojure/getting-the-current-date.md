---
title:    "Clojure: Få den nuvarande datumen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumen är en grundläggande uppgift som är nödvändig för många program. Det kan användas för att hålla koll på betalningar, schemalägga uppgifter och för att skapa tidsstämplar för aktiviteter.

## Hur man gör det

Ett sätt att få den nuvarande datumen i Clojure är genom att använda funktionen `(java.util.Date.)`, vilket skapar en ny instans av `java.util.Date`, som representerar den exakta tiden när den skapades.

```Clojure
(def now (java.util.Date.))
(println now)
```

Output: `Thu Jan 07 23:23:11 CET 2021`

Vi kan också använda `java.time` biblioteket för att få en mer lättläst formatering av den nuvarande datumen.

```Clojure
(import java.time.LocalDateTime)

(def now (LocalDateTime/now))
(println now)
```

Output: `2021-01-07T23:23:11.743`

Nu när vi har den nuvarande datumen som en variabel, kan vi manipulera och utföra olika operationer med det i vårt program.

## Djupdykning

Att få den nuvarande datumen i Clojure kan vara mer komplex beroende på vilken precision du behöver. För mer exakt tid, kan du använda funktionen `java.lang.System/currentTimeMillis` som returnerar en lång värde som representerar aktuell tid i millisekunder.

```Clojure
(def now (java.lang.System/currentTimeMillis))
(println now)
```

Output: `1610077491061`

Du kan också använda `java.time.Clock` för att få den nuvarande datumen i en viss tidszon. Bara importera klassen och skapa en ny instans med önskad tidszon.

```Clojure
(import java.time.Clock)

(def clock (Clock/systemUTC))
(def now (LocalDateTime/now clock))
(println now)
```

Output: `2021-01-07T22:23:11.743`

## Se även

- [JavaDoc för java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [JavaDoc för java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure.org dokumentation för hantering av datum och tid](https://clojuredocs.org/clojure.java-time)
- [En guide till Java Date och Time API i Clojure](https://www.baeldung.com/java-8-date-time-intro)