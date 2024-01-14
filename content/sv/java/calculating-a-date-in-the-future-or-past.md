---
title:    "Java: Beräkning av ett datum i framtiden eller förflutna"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara en värdefull färdighet i programmering. Det kan hjälpa dig att hantera schemaläggning, planering och andra aspekter av din kod. I det här blogginlägget kommer vi att utforska hur man enkelt kan göra detta i Java.

## Så här gör du

Först måste vi importera `LocalDate` klassen från `java.time` paketet:

```java
import java.time.LocalDate;
```

Därefter kan vi använda `LocalDate` klassens metoder för att beräkna ett datum i framtiden eller förflutna. Här är ett exempel på hur du kan beräkna ett datum 30 dagar efter dagens datum:

```java
LocalDate today = LocalDate.now();
LocalDate thirtyDaysLater = today.plusDays(30);
System.out.println("Datumet 30 dagar från idag är: " + thirtyDaysLater);
```

Output:

```
Datumet 30 dagar från idag är: 2021-09-20
```

På samma sätt kan vi använda `.minusDays()` metoden för att beräkna ett datum i förflutna. I följande exempel beräknar vi datumet 1 år tillbaka från dagens datum:

```java
LocalDate today = LocalDate.now();
LocalDate oneYearAgo = today.minusYears(1);
System.out.println("Datumet 1 år tillbaka är: " + oneYearAgo);
```

Output:

```
Datumet 1 år tillbaka är: 2020-08-22
```

Det finns även andra metoder som `.plusMonths()`, `.minusWeeks()` och så vidare som kan användas för att beräkna ett datum i framtiden eller förflutna baserat på månader, veckor eller till och med år.

## Deep Dive

För de som är intresserade av hur detta fungerar bakom kulisserna, så använder Java `ChronoUnit` klassen för att beräkna datumskillnaden och `Temporal` klassen för att lagra det beräknade datumen. Det är viktigt att notera att denna metod tar hänsyn till faktiska kalenderdatum för att säkerställa korrekta resultat.

Nu när du har lärt dig grunderna för att beräkna datum i Java, är det viktigt att använda detta på rätt sätt i din kod för att undvika eventuella fel.

## Se även

- [Oracle Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java LocalDate documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java 8 Datum och Tid: LocalDate](https://code-examples.net/sv/docs/java~8/java.time.localdate)