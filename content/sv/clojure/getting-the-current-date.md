---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:13:55.286068-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum i Clojure innebär att vi får tag på dagens datum. Det används för att logga händelser, spåra användaraktivitet, eller helt enkelt för funktioner som kräver dagens dato.

## How to:
För att hämta aktuellt datum i Clojure kan vi använda Java interoperability, eftersom Clojure körs på JVM. Här är ett exempel:

```Clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Använd funktionen
(println "Dagens datum är: " (get-current-date))
```

Sample output:

```
Dagens datum är: 2023-03-23
```

## Deep Dive
Clojure bygger på JVM, vilket innebär att Java's omfattande datum- och tidsbibliotek är tillgängligt. `LocalDate/now` hämtar det lokala datumet utan tid. Historiskt sätt, innan Java 8, användes `java.util.Date` men det var infamt för sitt komplexa API och timezone-frågor. Alternativ inkluderar användning av tredjepartsbibliotek som clj-time, som bygger på Joda-Time, men `java.time`-paketet är nu det föredragna valet för datumhantering i både Java och Clojure.

Det finns ett par olika alternativ när det gäller att hämta datum och tid i Clojure:

1. Använda `java.util.Calendar` för mer specifika krav.
2. Använda `java.time.ZonedDateTime` för datum och tid med tidszonsstöd.
3. Tredjepartspaketet clj-time för de som föredrar en Clojure-idiomatisk känsla.

Dock, med introduktionen av `java.time` i Java 8, har det blivit standarden för datum- och tidshantering.

## See Also
För mer information och relaterad läsning, ta en titt på följande källor:

- Clojure Documentation: https://clojure.org/
- Java 8 Date-Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- clj-time GitHub repository: https://github.com/clj-time/clj-time
