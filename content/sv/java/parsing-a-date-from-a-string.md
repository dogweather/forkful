---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:55.474699-07:00
simple_title:         "Tolka ett datum från en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att tolka ett datum från en sträng innebär att göra om textinformationen till ett datumformat som Java förstår. Programmerare behöver det för att hantera och manipulera datum, t.ex. för att spara i databaser eller jämföra med andra datum.

## How to:
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateString = "2023-04-01";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate parsedDate = LocalDate.parse(dateString, formatter);
        
        System.out.println(parsedDate);  // Output: 2023-04-01
    }
}
```

## Deep Dive
Innan Java 8, användes `SimpleDateFormat` för att tolka och formatera datum, men det var inte trådsäkert och lite klumpigt. `DateTimeFormatter` introducerades i Java 8 som en del av det nya datum- och tids-API:et i paketet `java.time`, som är både säkrare och mer intuitivt. 

Alternativt kan man använda tredjepartsbibliotek som Joda-Time, men med `java.time`-paketet är det sällan nödvändigt. Vad gäller implementeringsdetaljer gör formattering och tolkning av datum med `DateTimeFormatter` att du kan specificera ett mönster och även lokaliseringsinställningar med `Locale`, vilket är bra för att stödja internationella program.

## See Also
- [DateTimeFormatter dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [LocalDate dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time API guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
