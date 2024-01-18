---
title:                "Parsera ett datum från en sträng"
html_title:           "Java: Parsera ett datum från en sträng"
simple_title:         "Parsera ett datum från en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parsa" ett datum från en sträng handlar om att konvertera en textsträng till ett datumobjekt i Java. Detta är en vanlig uppgift för programmerare när de arbetar med data som innehåller datuminformation, till exempel vid hantering av tidsstämplar eller formatering av användarinput. 

## Så här gör du:
För att utföra parsning av ett datum från en sträng i Java, kan du använda metoden ".parse()" från klassen "SimpleDateFormat". Nedan finns ett exempel där en sträng representerande ett datum (i formatet "dd/mmm/yyyy") parsas och sedan skrivs ut på konsolen: 

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MMM/yyyy");
String dateString = "15/jan/2020";
Date date = sdf.parse(dateString);
System.out.println(date);
```

Output: 
```Thu Jan 15 00:00:00 EST 2020```

## Djupdykning:
Parsning av datum från strängar har funnits i Java sedan JDK 1.1, och används ofta tillsammans med klassen "Date" för att hantera datum och tidsstämplar. Det finns också flera andra sätt att utföra parsning av datum i Java, till exempel med hjälp av "DateTimeFormatter" från "java.time" paketet som introducerades i Java 8 för att hantera tidsrelaterade uppgifter på ett förbättrat sätt.

## Se även:
- Java Date and Time API: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Java SimpleDateFormat: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html