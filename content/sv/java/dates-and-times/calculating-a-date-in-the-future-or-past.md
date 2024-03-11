---
date: 2024-01-20 17:31:26.717069-07:00
description: "Att ber\xE4kna datum i framtiden eller f\xF6rflutna inneb\xE4r att du\
  \ r\xE4knar ut ett exakt datum f\xF6re eller efter en specifik punkt i tiden. Programmerare\
  \ g\xF6r detta\u2026"
lastmod: '2024-03-11T00:14:11.146539-06:00'
model: gpt-4-1106-preview
summary: "Att ber\xE4kna datum i framtiden eller f\xF6rflutna inneb\xE4r att du r\xE4\
  knar ut ett exakt datum f\xF6re eller efter en specifik punkt i tiden. Programmerare\
  \ g\xF6r detta\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna datum i framtiden eller förflutna innebär att du räknar ut ett exakt datum före eller efter en specifik punkt i tiden. Programmerare gör detta för att hantera bokningar, påminnelser, tidsbaserade händelser eller för att spåra hur lång tid något har tagit.

## Hur man gör:

Java inbyggda klasser som `LocalDate` och `Period` gör det enkelt. Kolla in koden:

```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalculator {
    public static void main(String[] args) {
        // Skapa dagens datum
        LocalDate today = LocalDate.now();
        
        // Lägg till 2 veckor
        LocalDate twoWeeksLater = today.plus(Period.ofWeeks(2));
        System.out.println("Datum om två veckor: " + twoWeeksLater);
        
        // Ta bort 5 dagar
        LocalDate fiveDaysAgo = today.minus(Period.ofDays(5));
        System.out.println("Datum för fem dagar sedan: " + fiveDaysAgo);
    }
}
```
Exempel på output:
```
Datum om två veckor: 2023-04-28
Datum för fem dagar sedan: 2023-04-11
```

## Fördjupning:

Förr använde Java `Date` och `Calendar` för tiddatumhantering. De var mutable och inte tidssäkra. Sedan Java 8 använder vi `LocalDate`, `LocalTime`, och `LocalDateTime` i `java.time`-paketet – de är omutbara och trådsäkra.

Ett alternativ är att använda `java.util.Calendar` för äldre Java-versioner, men det är knöligare och mer felbenäget. Joda-Time var en populär tredjepartsbibliotek före Java 8, men nu är `java.time` att föredra.

Detaljer i implementationen att notera är tidszonshantering (`ZonedDateTime`), och formatanpassning med `DateTimeFormatter` om du behöver visa datumen på olika sätt.

## Se även:

- Java-dokumentation för `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Oracle's tutorial om datum och tid: https://docs.oracle.com/javase/tutorial/datetime/
- ISO 8601 Datum och tidsstandarder: https://www.iso.org/iso-8601-date-and-time-format.html
- Joda-Time, för historisk kontext: http://www.joda.org/joda-time/
