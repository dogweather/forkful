---
title:                "Konvertering av datum till en sträng"
html_title:           "Clojure: Konvertering av datum till en sträng"
simple_title:         "Konvertering av datum till en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till varför man skulle vilja konvertera ett datum till en sträng i Clojure. Det kan vara för att visa datumet i ett visst format i en applikation, för att jämföra datum, eller för att spara datumet som en sträng i en databas.

## Hur man gör
Att konvertera ett datum till en sträng i Clojure är enkelt med hjälp av inbyggda funktioner som `format` och `str`. Här är ett exempel som visar hur man kan använda `format` för att skapa en sträng med dagens datum i formatet "YYYY-MM-DD".

```Clojure
(def today (java.util.Date.))           ; skapar en instance av klassen java.util.Date för dagens datum
(format "%1$tY-%1$tm-%1$td" today)      ; använder funktionen format för att skapa strängen "2021-07-12"
```

Man kan också använda funktionen `format` för att skapa en sträng med ett specifikt datum med hjälp av dess år, månad och dag som argument.

```Clojure
(format "%1$tY-%1$tm-%1$td" (java.util.Date. 1995 10 24))      ; skapar en sträng med datumet 1995-10-24
```

Om man bara vill ha en sträng med dagens datum utan någon formatering kan man använda funktionen `str`. Här är ett exempel på hur man kan använda `str` för att skapa en sträng med dagens datum i formatet "DD/MM/YYYY".

```Clojure
(str (-> (java.time.LocalDate/now)
          (.getDayOfMonth)
          (format "%02d")) "/" 
     (-> (java.time.LocalDate/now)
          (.getMonthValue)
          (format "%02d")) "/"
     (-> (java.time.LocalDate/now)
          (.getYear)))
```

Det finns också inbyggda funktioner som `date` och `today` som returnerar dagens datum som en java.util.Date eller java.time.LocalDate. Dessa kan också användas för att skapa en sträng med dagens datum.

```Clojure
(format "%1$tY-%1$tm-%1$td" (date))                    ; samma resultat som det första exemplet 
(str (-> (today)
         (.getDayOfMonth)
         (format "%02d")) "/"
    (-> (today)
         (.getMonthValue)
         (format "%02d")) "/"
    (-> (today)
         (.getYear)))                                   ; samma resultat som det sista exemplet
```

## Djupdykning
Att konvertera ett datum till en sträng i Clojure involverar först och främst att skapa en instance av java.util.Date eller java.time.LocalDate. Dessa är inbyggda klasser i Java som Clojure kan använda. Sedan använder man funktioner som `format` och `str` för att formatera och returnera en sträng med datumet.

Det finns många olika formateringsalternativ som man kan använda med `format` för att få datumet i önskad form. Till exempel kan man använda `%1$tl` för att få tiden i 12-timmarsformat eller `%1$tH` för 24-timmarsformat.

Det är också viktigt att notera att Clojure använder sig av Java's `SimpleDateFormat` för att formatera datum. Detta betyder att man kan använda sig av Java's dokumentation för att hitta flera formateringsalternativ och lära sig mer om hur man kan anpassa datumsträngen.

## Se även
- [Java 8 Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) - Dokumentation om Java's SimpleDateFormat som används av Clojure för att formatera datum.
- [clojure.java-time](https://github.com/dm3/clojure.java-time) - Ett bibliotek som förbättrar hanteringen av datum och tid i Clojure och ger möjlighet att använda Java's Date and Time API.