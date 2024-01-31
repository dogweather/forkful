---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:36:44.679545-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"

category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing en dato fra en streng er prosessen der du tar en tekstrepresentasjon av en dato og gjør om til et dato-objekt som Java forstår. Vi gjør det fordi data ofte kommer i tekstformat, og vi trenger å manipulere det som tid og dato.

## Hvordan gjøre det:
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateString = "2023-03-28";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate parsedDate = LocalDate.parse(dateString, formatter);
        
        System.out.println("Parsed date: " + parsedDate);
    }
}
```
Output:
```
Parsed date: 2023-03-28
```

## Dypdykk
I tidligere Java-versjoner brukte vi `SimpleDateFormat` fra `java.text`-pakken, men denne var feilutsatt og ikke trådsikker. Java 8 introduserte `java.time`, også kalt Date-Time API, som er bedre på alle måter. Det støtter ulike tidszoner, kan håndtere ulike kalendere, og er sikrere å bruke i en flertrådet kontekst.

Om du skulle trenge å parse en dato i en annen format, kan `DateTimeFormatter` tilpasses. For eksempel, hvis datoen er gitt som "28.03.2023", bruk da mønsteret `"dd.MM.yyyy"`. For eldre Java-versjoner eller spesielle behov, kan Joda-Time biblioteket være et godt alternativ, men anbefales ikke for nye prosjekter da `java.time` er foretrukket.

Implementasjonsdetaljer kan variere basert på hvordan datoen er formatert. Pass på å håndtere `DateTimeParseException` som kan kastes hvis strengen ikke passer med formatteringsmønsteret. Det er også mulig å la brukeren definere formatet dynamisk, men da øker kompleksiteten rundt feilhåndtering og datavalidering.

## Se også:
- [Date and Time API guide](https://docs.oracle.com/javase/tutorial/datetime/)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
