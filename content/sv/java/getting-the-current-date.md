---
title:                "Att hämta den aktuella datumen"
html_title:           "Java: Att hämta den aktuella datumen"
simple_title:         "Att hämta den aktuella datumen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få dagens datum är ett vanligt behov inom programmering och det handlar helt enkelt om att få reda på dagens datum. Detta kan användas för att planera och organisera program eller för att visa aktuell tid och datum i en användarapplikation.

## Så här gör du:
För att få dagens datum i Java kan du använda Date-klassen och dess inbyggda metoder. Nedan är ett exempel på hur du kan implementera detta i din kod:

```Java
import java.util.Date;

// Skapar ett Date-objekt som innehåller dagens datum
Date date = new Date();

// Skriver ut dagens datum i konsolen
System.out.println(date);
```

Detta kommer att ge följande utdata på konsolen:

```
Mon May 24 15:50:32 CEST 2021
```

Notera att datumet som visas kan variera beroende på din tidszon.

## Djupdykning:
Historiskt sett var det vanligt att programmerare skapade egna funktioner för att få dagens datum, men med Java 8 introducerades nya tid- och datum API:er som gjorde det enklare att få tag på dagens datum och utföra andra manipuleringar med datum och tid. Alternativt kan du också använda Calendar-klassen istället för Date-klassen, men den anses nu vara föråldrad.

En annan anledning till att andra programmerare kan behöva få dagens datum kan vara för att genomföra timeturval och omräkningar i olika tidsenheter. Som tur är har Java inbyggda funktioner för detta, till exempel LocalDate, LocalDateTime och ZonedDateTime.

## Se även:
- [Java Date Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Date.html)
- [Java 8 Time API Tutorial](https://www.baeldung.com/java-8-date-time-intro)
- [Java LocalDate Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Java Calendar Class Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Calendar.html)