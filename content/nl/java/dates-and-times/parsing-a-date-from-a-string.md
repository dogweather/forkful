---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:01.327141-07:00
description: "Het ontleden van een datum uit een string betekent het omzetten van\
  \ geschreven tekst naar een Date-object dat een programma kan gebruiken. Programmeurs\u2026"
lastmod: '2024-02-25T18:49:48.033318-07:00'
model: gpt-4-0125-preview
summary: "Het ontleden van een datum uit een string betekent het omzetten van geschreven\
  \ tekst naar een Date-object dat een programma kan gebruiken. Programmeurs\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het ontleden van een datum uit een string betekent het omzetten van geschreven tekst naar een Date-object dat een programma kan gebruiken. Programmeurs doen dit om gebruikersinvoer of in leesbare formaten opgeslagen gegevens te begrijpen.

## Hoe:

Java heeft een klasse `java.time.format.DateTimeFormatter` voor dit soort werk. Hier is hoe je ermee omgaat.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {

    public static void main(String[] args) {
        String dateString = "2023-03-15";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Ontlede datum is: " + date);
        } catch (DateTimeParseException e) {
            System.err.println("Oeps, datum was in het verkeerde formaat!");
        }
    }
}
```

Draai dit kleine stukje en je zult zien:

```
Ontlede datum is: 2023-03-15
```

## Diepgaande duik

Lang voordat `java.time` binnenkwam met Java 8 in 2014, worstelden mensen met `java.util.Date` en `SimpleDateFormat`. Deze oudjes zijn niet alleen vijandig tegenover threads, maar ook een hoofdpijn om te gebruiken met hun tijdzone eigenaardigheden.

Tegenwoordig is `java.time` de hit. Het is thread-safe, onveranderlijk (geen stiekeme wijzigingen), en duidelijker in intentie. Bovendien kun je kiezen uit een set van vooraf gedefinieerde formatters of je eigen patroon samenstellen.

Alternatieven, vraag je? Bibliotheken zoals Joda-Time baanden de weg, maar aangezien java.time zwaar leent van zijn ideeën, hebben de meesten hun hoed afgenomen ten gunste van de standaard bibliotheek.

Onder de motorkap doet het ontleden met `DateTimeFormatter` wat zwaar werk. Het controleert patronen, valideert invoer, handelt uitzonderingen af en levert een glanzende `LocalDate`, `LocalTime`, of zelfs `ZonedDateTime` af, afhankelijk van wat je zoekt.

## Zie ook

- De officiële Java documentatie voor `java.time.format.DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Oracle's Java tutorials, inclusief datum en tijd: https://docs.oracle.com/javase/tutorial/datetime/
