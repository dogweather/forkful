---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Klä på sig med Datum i Clojure

## Vad & Varför?

Att få det aktuella datumet handlar om att hämta den aktuella tiden, ned till sekunden, för ögonblicket när koden körs. Det är ovärderligt för att logga, tidstämpla, beräkna tidsskillnader, och många fler applikationer.

## Hur göra:

Att hämta det nuvarande datumet i Clojure är överraskande simpelt. Vi kan använda `java.util.Date` klassen. Här är hur:

```Clojure
(import 'java.util.Date)
(def current-date (Date.))
```

När du kör ovanstående kod, kommer `current-date` att vara ett `java.util.Date` objekt som representerar den exakta tidpunkten när objektet skapades. Pröva och skriv ut det:

```Clojure
(println current-date)
```

Du borde se något ungefär som det här:
`Tue Sep 28 11:43:22 CEST 2021`

## Fördjupning

Att hämta det aktuella datumet kanske verkar rakt på sak, men det finns en del saker att tänka på. För det första, `java.util.Date`-objekt representerar ett exakt ögonblick i tiden, ner till millisekunder. Detta gör det perfekt för loggning och tidstämpling, men om du bara behöver datumet kan det bli knepigt.

För detta ändamål kan du använda `java.time.LocalDate` klassen:

```Clojure
(import 'java.time.LocalDate)
(def only-date (.toString LocalDate/now))
```

Detta kommer att returnera dagens datum som en sträng i formatet `yyyy-mm-dd`.

Histories sett har Java haft problem med datum- och tidsmanipulering, men tack vare valfriheten i Clojure och tillgången till Java's inbyggda klasser, är att hantera datum och tid i Clojure inget att oroa sig för.

## Se också

För mer information och alternativ för att hämta och hantera datum och tid i Clojure, se följande resurser:

- Official Clojure Documentation: [https://clojure.org/guides/dates_and_time](https://clojure.org/guides/dates_and_time)
- Clojure Cookbook: [https://www.braveclojure.com/zombie-metaphysics/](https://www.braveclojure.com/zombie-metaphysics/)
  
Där har du det, ett snabbt sätt att hämta det aktuella datumet i Clojure!