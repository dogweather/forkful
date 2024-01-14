---
title:    "Clojure: Att få aktuellt datum"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få dagens datum är en vanlig och nödvändig uppgift inom programmering. Genom att inkludera aktuellt datum i applikationer eller script kan man bland annat hålla reda på skapade dokument eller hålla koll på deadlines.

## Hur man gör
För att få aktuellt datum i Clojure behöver man först importera biblioteket "java.time". Sedan kan man använda sig av funktionen "LocalDate/now" för att få dagens datum.

```Clojure
(import java.time.LocalDate)

(def current-date (LocalDate/now))
```

Om man nu till exempel vill skriva ut dagens datum på ett snyggt sätt kan vi använda "println" tillsammans med "clj-time", ett bibliotek som erbjuder funktioner för att arbeta med datum.

```Clojure
(import java.time.LocalDate)
(require '[clj-time.core :refer [format]])
(def current-date (LocalDate/now))
(println (format current-date "Datum: yyyy-MM-dd"))
```

Detta kommer att skriva ut något liknande "Datum: 2021-10-22".

För mer komplexa manipulationer av datum, som att lägga till eller ta bort dagar, månader eller år kan man använda sig av "clj-time" tillsammans med funktionen "plus" eller "minus".

## Djupdykning
Att få aktuellt datum är en grundläggande uppgift i Clojure, men det finns mycket mer man kan göra med datum och tidsuppgifter. Genom att använda sig av olika bibliotek, som "clj-time", kan man enkelt hantera olika tidszoner, arbeta med intervall och utföra omvandlingar mellan olika format.

## Se även
- [ClojureDocs: LocalDate/now](https://clojuredocs.org/clojure.java-time.local-date/now)
- [ClojureDocs: clj-time](https://clojuredocs.org/clj-time)
- [Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)