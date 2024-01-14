---
title:    "Clojure: Omvandla ett datum till en sträng"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera datum till en sträng är en vanlig uppgift inom programmering eftersom det gör det enkelt att representera datum i ett läsbart format.

## Hur man gör det
Att konvertera ett datum till en sträng kan göras enkelt i Clojure med hjälp av den inbyggda funktionen `str`. Här är ett exempel som visar hur man kan konvertera dagens datum till en sträng:

```Clojure
(str (java.util.Date.)) ;=> "Tue Aug 17 00:00:00 UTC 2021"
```

Om du vill ha ett specifikt format för datumet kan du använda sig av funktionen `format`. Nedan är ett exempel på hur man kan konvertera datumet till formatet DD-MM-YYYY:

```Clojure
(require '[clojure.string :as str])
(str/join "-" (format "%td" (java.util.Date.)) (format "%tm" (java.util.Date.)) (format "%tY" (java.util.Date.))) ;=> "17-08-2021"
``` 

## Djupdykning
För att få en djupare förståelse för hur datum kan konverteras till strängar kan det vara bra att titta på olika typer av format och hur de skapas. Till exempel kan man använda sig av funktionen `strf` för att formatera datumet baserat på ett visst mönster. Det finns också olika bibliotek som tillhandahåller ytterligare funktioner för att konvertera datum.

## Se också
- [Clojure dokumentation för str](https://clojuredocs.org/clojure.core/str)
- [Javadoc för java.util.Date](https://docs.oracle.com/javase/9/docs/api/java/util/Date.html)
- [Clojure-datum bibliotek](https://github.com/clj-datetime/clj-datetime)