---
title:                "Java: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Varför
Att kunna konvertera datum till strängar är en vanlig uppgift i Java-programmering. Det är ett användbart verktyg för att visa datum i ett läsbart format och för att utföra olika jämförelser mellan datum.

##Hur man gör det
För att konvertera ett datum till en sträng, måste vi först skapa ett objekt av typen `Date` och sedan använda Java's `SimpleDateFormat` för att definiera det önskade formatet på vår sträng. För att illustrera detta, ta en titt på följande kodexempel:

```java
Date datum = new Date(); // skapar ett nytt datumobjekt
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy"); // definierar formatet för vår sträng
String datumSomStrang = sdf.format(datum); // konverterar datumet till en sträng
System.out.println(datumSomStrang); // ger utdata "27/04/2021"
```

Som du kan se i exemplet ovan, så behöver vi först definiera ett format för vår sträng genom att använda `SimpleDateFormat`. I detta fall har vi valt att visa datumet i formatet dag/månad/år, men du kan välja vilket format som helst baserat på dina behov. Sedan använder vi metoden `format()` för att konvertera vårt datumobjekt till en sträng. Slutligen skriver vi ut strängen till konsolen med `System.out.println()`.

Det är också värt att nämna att Java har inbyggda metoder för att konvertera datum till andra vanliga format, som `toString()` för att konvertera till sträng i standardformat eller `getTime()` för att konvertera till antalet millisekunder sedan Januari 1, 1970.

##Djupdykning
För mer avancerade behov, som att konvertera datum från en tidszon till en annan eller inkludera tidskomponenter som timmar och minuter, kan du behöva använda andra verktyg som `Calendar` och `TimeZone` i kombination med `SimpleDateFormat`. Du kan också läsa mer om olika formatteringsalternativ i Java's dokumentation för `SimpleDateFormat`.

Ytterligare en viktig sak att tänka på är att hantera eventuella fel som kan uppstå under konverteringen. Till exempel kan ett ogiltigt datum orsaka en `ParseException`. Det är alltid bra att lägga till lämplig felhantering i din kod för att undvika problem.

##Se också
- [Java's Date-klass](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java's SimpleDateFormat-klass](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Format-strängar för SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html#dateformat) (på engelska)
- [Java's TimeZone-klass](https://docs.oracle.com/javase/8/docs/api/java/util/TimeZone.html)
- [Java's Calendar-klass](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)