---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i Java är att erhålla det nuvarande datumet och tiden från systemklockan. Det är nyckeln till att spåra händelser, logga data och hantera tidsspecifika funktioner i applikationer.

## Hur man gör:
För att hämta det nuvarande datumet i Java använder vi *java.time.LocalDate* klass. Här är ett kort exempel:

```Java
import java.time.LocalDate;

public class Main {
   public static void main(String[] args) {
      LocalDate date = LocalDate.now();
      System.out.println("Dagens datum är: " + date);
   }
}
```

När du kör detta program, kommer outputen att vara:

```Java
Dagens datum är: 2022-10-01
```

Observera att utskriften kan variera beroende på när du kör koden.

## Fördjupning
Historiskt sett har Java hanterat datum med hjälp av `java.util.Date` och `java.util.Calendar` klasser. Men med Java 8 introducerades ett nytt datum- och tids-API som är mer robust och lätt att använda.

Alternativ till `LocalDate` inkluderar `LocalDateTime` och `ZonedDateTime`.

* `LocalDateTime` ger både datum och tid, utan tidszon.
* `ZonedDateTime` ger datum, tid och tidszon.

Dessa koder visar hur man använder dem:

```Java
import java.time.LocalDateTime;
import java.time.ZonedDateTime;

LocalDateTime dateTime = LocalDateTime.now();
ZonedDateTime zonedDateTime = ZonedDateTime.now();
System.out.println("Dagens datum och tid: " + dateTime);
System.out.println("Dagens datum, tid och zon: " + zonedDateTime);
```

Värt att nämna är att `LocalDate.now()` internt anropar `Clock.systemDefaultZone()`, vilket innebär att datumet som returneras är aktuellt datum enligt standardtidszonen i det system där programmet körs.

## Se också
Länkar till relaterade resurser:

* [Java SE 8 Date and Time (Oracle)](https://docs.oracle.com/javase/tutorial/datetime/)

* [Java LocalDate (Baeldung)](https://www.baeldung.com/java-8-date-time-intro)

* [Java Date Time (TutorialsPoint)](https://www.tutorialspoint.com/java8/java8_datetime_api.htm)