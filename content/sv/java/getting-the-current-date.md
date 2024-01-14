---
title:                "Java: Att få den aktuella datumen"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumet är en vanlig uppgift inom programmering, oavsett om man skapar en kalenderapplikation eller behöver logga när en viss händelse inträffade. Genom att lära sig hur man kan hämta den aktuella datumet i Java kan man enkelt lägga till denna funktionalitet i sitt program.

## Så här gör du

Att få den nuvarande datumet i Java är en enkel process. Du behöver bara använda klassen `Date` och dess metod `toString()` för att få det aktuella datumet. Här är ett exempel på kod som visar hur man kan göra det:

```Java
// Importera Date-klassen
import java.util.Date;

// Hämta aktuellt datum och spara i en Date-variabel
Date nuvarandeDatum = new Date();

// Använd toString() för att konvertera datumet till en textsträng
String datumText = nuvarandeDatum.toString();

// Skriv ut resultatet
System.out.println("Nuvarande datum är: " + datumText);
```

#### Resultat:
Nuvarande datum är: Sat May 08 19:17:22 CEST 2021

Som du kan se är det inte svårt att få det aktuella datumet i Java. Om du vill kan du också formatera utskriften för att visa datumet på ett mer läsbart sätt. Du kan till exempel använda klassen `SimpleDateFormat` för att välja ett visst datumformat. Här är ett exempel på hur du kan göra det:

```Java
// Importera Date- och SimpleDateFormat-klasserna
import java.util.Date;
import java.text.SimpleDateFormat;

// Hämta aktuellt datum och spara i en Date-variabel
Date nuvarandeDatum = new Date();

// Använd SimpleDateFormat för att välja datumformat
SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");

// Använd formateratDatum för att få datumet i rätt format
String formateratDatum = sdf.format(nuvarandeDatum);

// Skriv ut resultatet
System.out.println("Nuvarande datum är: " + formateratDatum);
```

#### Resultat:
Nuvarande datum är: 2021/05/08

## Deep Dive

Förutom att få det aktuella datumet finns det en mängd andra saker du kan göra med Date-klassen i Java. Du kan till exempel jämföra datum, få ut specifika delar av datumet (som dag, månad och år) och även skapa nya datum baserat på ett visst antal dagar från ett given datum. Det finns också andra klasser som `Calendar` och `LocalDate` som kan vara användbara om du behöver hantera datum på ett mer avancerat sätt.

## See Also

För mer information om vad man kan göra med datum i Java, kolla in dessa resurser:

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Comparing Dates in Java](https://www.baeldung.com/java-date-compare)