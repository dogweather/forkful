---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:15:00.454631-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet innebär att få reda på exakt vilket datum det är just nu. Programmerare gör detta för att logga händelser, tidsstämpla data, eller för funktioner som kräver dagens datum.

## Så här gör du:
Java har inbyggda klasser för datum och tid. Den rekommenderade klassen för nuvarande versionen är `LocalDate`. Här är hur du använder den:

```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Today's Date: " + today);
    }
}
```

Kör du detta kommer utdatan att visa dagens datum:

```
Today's Date: 2023-04-12
```

## Djupdykning:
Förr använde Java `Date` och `Calendar`-klasserna, men de hade brister, som att vara svåra att använda och icke-trådsäkra. Java 8 introducerade `java.time`-paketet, även känt som JSR-310, som en del av en större uppgradering av datum- och tidshantering.

Alternativt, för mer specifika tidsbehov, kan `LocalDateTime` eller `ZonedDateTime` klasser användas. Dessa inkluderar tidsinformation och tidszonsstöd.

Dessa klasser bygger på `Instant`-klassen som representerar ett tidspunkt i GMT. För att få en instans med nuvarande datum och tid, kan du göra så här:

```java
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class PreciseDateExample {
    public static void main(String[] args) {
        Instant now = Instant.now();
        ZonedDateTime currentDateTime = now.atZone(ZoneId.systemDefault());
        System.out.println("Current Date and Time with Zone: " + currentDateTime);
    }
}
```

Som ger ett resultat med tidszonsinformation:

```
Current Date and Time with Zone: 2023-04-12T14:20:30.123456+02:00[Europe/Stockholm]
```

## Se också:
- [Oracle’s Java Date Time tutorials](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java 8 Date Time API: JSR-310](https://jcp.org/en/jsr/detail?id=310)
- [Java's `Instant` class documentation](https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html)
