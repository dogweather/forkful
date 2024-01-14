---
title:                "Java: Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna datum i framtiden eller förflutna kan vara en användbar funktion för att hantera tidssensitive information eller att planera för händelser i framtiden. Det kan vara särskilt användbart för att upprätthålla deadlines eller för att hålla koll på åldern för olika händelser.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna behöver vi använda oss av en kombination av datatyper och inbyggda funktioner i Java. Låt oss ta en titt på ett exempel för att förtydliga det:

````java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculator {
  public static void main(String[] args) {
    // Vi väljer ett specifikt datum att utgå ifrån
    LocalDate startDate = LocalDate.of(2020, 5, 15);
    
    // Vi använder oss av en DateTimeFormatter för att definiera formatet på datumet
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
    
    // Vi kan sedan använda oss av LocalDate's metod plusDays för att beräkna ett datum i framtiden eller förflutna
    // I det här fallet lägger vi till 10 dagar till vårt startdatum
    LocalDate futureDate = startDate.plusDays(10);
    
    // Vi kan också använda minusDays för en beräkning i förflutna
    LocalDate pastDate = startDate.minusDays(10);
    
    // Slutligen skriver vi ut resultaten i det definierade formatet med hjälp av DateTimeFormatter
    System.out.println(formatter.format(futureDate));
    System.out.println(formatter.format(pastDate));
  }
}
````

Körningsresultat:

```
25/05/2020
05/05/2020
```

Som vi kan se kan vi enkelt beräkna ett datum i framtiden eller förflutna genom att använda oss av Java's inbyggda funktioner.

## Djupdykning

Om du vill ha mer avancerade beräkningar kan du också använda dig av andra klasser som är tillgängliga i java.time-paketet, såsom Period och Duration. Dessa klasser tillåter mer exakta beräkningar beroende på dina specifika behov.

Du kan också använda dig av olika metoder för att manipulera datumet, såsom plusWeeks(), plusMonths() eller plusYears(). Utforska dessa funktioner för att hitta den bästa lösningen för ditt specifika projekt.

## Se också

- [Java Dokumentation: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial: Beräkna datum i Java](https://www.baeldung.com/java-date-difference)
- [Java Date and Time API – Handling of Time and Date](https://www.baeldung.com/java-8-date-time-intro)