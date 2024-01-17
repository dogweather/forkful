---
title:                "Konvertera ett datum till en sträng"
html_title:           "Clojure: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Konvertering av ett datum till en sträng är en vanlig uppgift för programmerare. Det innebär att man omvandlar ett datum, som vanligtvis lagras som ett numeriskt värde, till en format som kan läsas av och användas i en sträng. Detta är viktigt eftersom det gör det lättare för utvecklare att hantera datum och tid i sina program.

## Så här gör du:

```Clojure 
; Konvertering av ett datum till en sträng i Clojure
(require '[clojure.java-time :as t])
(import '[java.time.format DateTimeFormatter])
 
; Skapa ett datumobjekt
(def date (t/local-date 2021 06 28))
 
; Konvertera till en sträng med formatet DD-MM-YYYY
(t/format date "dd-MM-yyyy")
; -> "28-06-2021"
 
; Konvertera till en sträng med formatet YYYY/MM/DD
(t/format date "yyyy/MM/dd")
; -> "2021/06/28"
```

## Utforska Djupet:

Att konvertera datum till sträng är en vanlig uppgift i många programmeringsspråk, eftersom datum är en grundläggande datatyp som används i många applikationer. Innan moderna språkhanteringsfunktioner blev vanliga, var det vanligt att manuellt konvertera datum till strängar genom att använda olika funktioner och manipulationer.

I dag finns det många alternativ för att konvertera datum till strängar, då de flesta programmeringsspråk inkluderar inbyggda funktioner eller bibliotek som kan hantera dessa uppgifter. I Clojure är det vanligt att använda java.time-paketet för att hantera datum och tider, och det finns många hjälpfunktioner som gör det enkelt att konvertera mellan olika format. 

## Se även:

- [Clojure's java.time documentation](https://clojure.github.io/java.time/), som innehåller mer information om hantering av datum och tider i Clojure.
- [Java's SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html), som ger en djupare förståelse för hur datum och tid kan hanteras i Java och Clojure.