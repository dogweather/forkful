---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att omvandla en strängrepresentation av ett datum till ett daumsobjekt som datorn kan använda. Programmerare gör det för att manipulera och beräkna datum i sina program.

## Så här gör du:

För att konvertera stränger till datum i Clojure, använd biblioteket `java.time`. Nedan hittar du exempelkod:

```Clojure
(import 'java.time.LocalDate)
(import 'java.time.format.DateTimeFormatter)

(defn string-to-date [s]
  (.parse LocalDate 
    (DateTimeFormatter/ofPattern "yyyy-MM-dd") s))
```

Kör den här koden med en datumsträng som "2022-05-06", så får du ett resultat som:
```Clojure
;; Använd funktionen
(string-to-date "2022-05-06")
```
`=> #object[java.time.LocalDate 0x78afebef "2022-05-06"]`

## Fördjupning

Dateringsparsning från strängar är en vanlig uppgift i programmering. Det historiska behovet av denna funktion sträcker sig tillbaka till de tidigaste dagarna av databehandling, när datum ofta lagrades som textstränger. 

Alternativen till det ovan beskrivna sättet att hantera datum i Clojure inkluderar användning av Joda-Time biblioteket (mer kraftfullt men mer komplext) eller innebyggda datum och tidstyp för Clojure (bra för grundläggande användning, men inte lika funktionell som de tidigare). 

Konvertering av sträng till datum i Clojure sker genom "java.time.LocalDate.parse()", vilket gör en konsekvent representation av ett datum utan tid eller tidszon. 

## Se även:

- [Java 8's Date and Time library](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
- [Clojure date-time libraries](https://clojure.org/api/cheatsheet)