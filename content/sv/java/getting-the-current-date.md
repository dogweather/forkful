---
title:    "Java: Hämta aktuellt datum"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumen kan vara en användbar funktion i många programmeringsprojekt. Det kan användas för att hålla reda på när en viss händelse inträffade, skapa unika filnamn eller helt enkelt för att visa den aktuella datum och tid i ett program. Oavsett vilket behov du har kan Java erbjuda en enkel lösning för att få den aktuella datumen.

## Hur man får den aktuella datumen i Java

För att få den aktuella datumen i Java, finns det flera olika metoder som kan användas. Ett sätt är att använda den inbyggda klassen "Date" tillsammans med "SimpleDateFormat" för att konvertera datumen till önskad format.

```Java
// Importera Date och SimpleDateFormatklasserna
import java.util.Date;
import java.text.SimpleDateFormat;

// Skapa ett nytt Date-objekt
Date date = new Date();

// Skapa ett SimpleDateFormat-objekt med önskat format
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");

// Konvertera datumet till en sträng
String currentDate = dateFormat.format(date);

// Skriv ut strängen med det aktuella datumet
System.out.println("Idag är det " + currentDate);
```

Detta kommer att ge följande utmatning, baserat på det aktuella datumet när kodblocket körs:

```
Idag är det 17/09/2021
```

## Djupdykning

Java har också ett alternativt sätt att få den aktuella datumen genom att använda klassen "LocalDate" från "java.time" paketet. Detta är en mer modern och förbättrad metod, som är mer lättläst och enkel att använda.

```Java
// Importera LocalDate-klassen
import java.time.LocalDate;

// Skapa ett LocalDate-objekt
LocalDate today = LocalDate.now();

// Skriv ut det aktuella datumet
System.out.println("Idag är det " + today);
```

Detta kommer att ge en utmatning på följande format:

```
Idag är det 2021-09-17
```

En annan användbar funktion för att få den aktuella datumen är att hämta ett specifikt datum, till exempel en vecka bakåt eller framåt från den aktuella datumen. Detta kan göras genom att använda metoden "plusDays()" tillsammans med "LocalDate".

```Java
// Skapa ett LocalDate-objekt för idag
LocalDate today = LocalDate.now();

// Skapa ett LocalDate-objekt för datumet en vecka bakåt från idag
LocalDate oneWeekAgo = today.plusDays(-7);

// Skapa ett LocalDate-objekt för datumet en vecka framåt från idag
LocalDate oneWeekFromNow = today.plusDays(7);

// Skriv ut de olika datumen
System.out.println("Idag är det " + today);
System.out.println("Förra veckan var det " + oneWeekAgo);
System.out.println("Nästa vecka kommer det att vara " + oneWeekFromNow);
```

Detta ger följande utmatning:

```
Idag är det 2021-09-17
Förra veckan var det 2021-09-10
Nästa vecka kommer det att vara 2021-09-24
```

## Se även

För mer information om hur man hanterar datum och tid i Java, kolla gärna in följande länkar:

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [How to get current date/time in Java](https://www.baeldung.com/java-get-current-date-time)
- [Java Date and Time Tutorial](https://www.tutorialspoint.com/java/java_date_time.htm)

Jag hoppas att denna guide har varit till hjälp för att få det aktuella datumet i Java. Lycka till med dina programmeringsprojekt!