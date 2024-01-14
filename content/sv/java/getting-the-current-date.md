---
title:                "Java: Att få den nuvarande datumen"
simple_title:         "Att få den nuvarande datumen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet är en användbar funktion i många olika program och applikationer. Det kan hjälpa till att organisera och strukturera data, och det kan även användas för att visa korrekt datum och tid för användarna.

## Så här gör du

För att hämta den aktuella datumet i Java, finns det flera olika metoder att använda beroende på vad du behöver. Nedan finns två olika kodexempel och den resulterande utgången för att hjälpa dig att förstå processen.

```Java
// Hämta datumet som ett Date-objekt
Date date = new Date(); // skapar ett nytt Date-objekt med aktuellt datum och tid
System.out.println(date); // skriver ut aktuellt datum och tid

// Konvertera datum till en sträng
SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy"); // skapar ett SimpleDateFormat-objekt med önskat format
String dateString = format.format(date); // konverterar datumet till en sträng enligt det valda formatet
System.out.println(dateString); // skriver ut datumet som en sträng
```

Output: 
Thu Jul 22 22:33:24 CEST 2021
22/07/2021

Som du kan se är det enkelt att hämta och konvertera den aktuella datumet till olika format som passar dina behov.

## Djupdykning

Java har flera olika klasser och metoder som kan användas för att hantera datum och tid. En av de vanligaste är Date-klassen som representerar ett specifikt datum och klockslag. Men det finns även andra klasser som kan vara användbara beroende på vad du behöver göra, såsom Calendar och LocalDate.

Det är viktigt att notera att datum och tid kan påverkas av olika faktorer som tidszoner och sommartid. Det är därför viktigt att ha detta i åtanke när du hämtar den aktuella datumet och hanterar datum och tid i dina program.

## Se även

Här är några användbara länkar för att lära dig mer om hur man hanterar datum och tid i Java:

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java Calendar Class](https://www.geeksforgeeks.org/java-util-calendar-class-java/)
- [Java 8 LocalDate Class](https://www.baeldung.com/java-8-date-time-intro)