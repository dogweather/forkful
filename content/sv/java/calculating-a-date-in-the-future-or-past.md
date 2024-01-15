---
title:                "Beräkning av ett datum i framtiden eller i det förflutna"
html_title:           "Java: Beräkning av ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller i det förflutna"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Det kan finnas många olika anledningar till varför man skulle vilja räkna ut ett datum i framtiden eller i det förflutna. Det kan vara för att planera evenemang, för att hålla reda på födelsedagar eller för att beräkna hur lång tid det är kvar till ett visst datum.

## Hur man gör det
Det finns flera sätt att beräkna ett datum i framtiden eller i det förflutna i Java. Ett enkelt sätt är att använda klassen `LocalDate` och dess metod `plusDays` eller `minusDays` beroende på om man vill räkna framåt eller bakåt i tiden. Till exempel:

```Java
LocalDate today = LocalDate.now(); // dagens datum
LocalDate tomorrow = today.plusDays(1); // räkna ut datumet för imorgon
LocalDate yesterday = today.minusDays(1); // räkna ut datumet för igår
System.out.println("Imorgon är det: " + tomorrow); // skriver ut: Imorgon är det: 2021-09-07
System.out.println("Igår var det: " + yesterday); // skriver ut: Igår var det: 2021-09-05
```

Man kan också använda klassen `LocalDateTime` om man även vill inkludera tid i beräkningen, eller klassen `ZonedDateTime` om man vill hantera olika tidszoner.

## Djupdykning
När man beräknar ett datum i framtiden eller i förflutna är det viktigt att ta hänsyn till eventuella skillnader i tidszoner och sommartid. Det kan också vara bra att använda sig av andra metoder som `plusMonths` eller `plusYears` beroende på vilken precis precision man behöver i beräkningen. Det finns också möjlighet att använda sig av andra klasser såsom `java.util.Calendar` eller `java.time.Period` för att göra mer avancerade beräkningar.

## Se även
- [Java's LocalDate Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java's LocalDateTime Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java's ZonedDateTime Class](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
- [Java's Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java's Period Class](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)