---
title:    "Java: Så omvandlar du ett datum till en sträng"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara en användbar funktion för att visa datum på ett mer läsbar sätt. Det kan också vara användbart när man lagrar datum i en databas eller skapar rapporter som involverar datum.

## Så här gör du

```java
// Skapa ett Date-objekt
Date datum = new Date();

// Skapa ett SimpleDateFormat-objekt för att formatera datumet
SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");

// Konvertera datumet till en sträng med hjälp av SimpleDateFormat
String datumSomSträng = format.format(datum);

// Skriv ut strängen
System.out.println(datumSomSträng);

// Output: 01/01/2020
```

I det här exemplet skapar vi ett Date-objekt som representerar dagens datum. Sedan skapar vi ett SimpleDateFormat-objekt och specificerar vilken formatmall vi vill använda för att konvertera datumet till en sträng. Sedan använder vi format.format () -metoden för att konvertera datumet till en sträng. Vi kan också specificera olika formatmallar beroende på hur vi vill att datumet ska visas. Till exempel kan vi använda "MM/dd/yyyy" för att visa månad, dag och år i stället för dag, månad och år som i exemplet ovan.

## Djupdykning

När vi konverterar ett datum till en sträng händer det mycket bakom kulisserna. Java har en inbyggd klass som heter Date som representerar ett specifikt datum och tid. När vi skapar ett Date-objekt får det en noll-tid som är antalet millisekunder från "epoch" (1 januari 1970 00:00:00 UTC) fram till det specifika datumet och tiden. När vi sedan använder SimpleDateFormat-klassen för att konvertera datumet till en sträng använder den denna "epoch" för att räkna ut det korrekta datumet baserat på det specificerade formatet.

## Se även

- [Java SimpleDateFormat-dokumentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date-dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)