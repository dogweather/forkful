---
title:                "Omvandla ett datum till en sträng"
html_title:           "Java: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
När vi pratar om att konvertera ett datum till en sträng i Java, så menar vi att omvandla formatet på datumet från ett datumobjekt till en textsträng. Det här är ett vanligt förekommande steg inom programmering när vi behöver använda datum i vår kod. Genom att konvertera datumet till en sträng, kan vi enkelt manipulera och visa det på önskat sätt.

## Så här gör du:
```Java
// Skapa ett datumobjekt
Date date = new Date();

// Med hjälp av SimpleDateFormat klassen kan vi konvertera datumet till en sträng enligt önskat format. Här använder vi formatet "dd/MM/yyyy".
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");

// Använd format-metoden för att konvertera datumet och lagra det som en sträng i en variabel.
String dateString = dateFormat.format(date);

// Skriv ut resultatet
System.out.println("Datumet som en sträng: " + dateString);
```

Output:
```
Datumet som en sträng: 24/06/2021
```

## Djupdykning:
Att konvertera ett datum till en sträng är ett vanligt förekommande problem inom programutveckling. Historiskt sett så användes den inbyggda Date-klassen för hantering av datum i Java, men den har visat sig vara besvärlig och problematisk. Numera är det rekommenderat att istället använda Joda-Time biblioteket eller Java 8's Date and Time API för att hantera datum och tidsberäkningar.

## Se även:
- [Java documentation for SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Joda-Time documentation](https://www.joda.org/joda-time/)
- [Java 8 Date and Time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)