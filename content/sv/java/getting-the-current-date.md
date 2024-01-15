---
title:                "Att hämta aktuellt datum"
html_title:           "Java: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet i ett program är en viktig funktion som kan vara användbar i många olika sammanhang. Oavsett om du behöver visa den aktuella datumet för användaren eller använda det för att utföra beräkningar eller jämförelser, är det viktigt att veta hur man gör detta korrekt i Java.

## Hur man gör det

För att hämta den aktuella datumet i Java, används klassen `java.util.Date` och dess metod `getDate()` tillsammans med `java.text.SimpleDateFormat` för att formatera datumet enligt önskat format. Här är ett exempel på hur man skulle kunna implementera detta i en Java-applikation:

```java
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.Locale;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Skapa ett objekt av typen Date
        Date currentDate = new Date();
        
        // Ange önskat datumformat
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        
        // Hämta den aktuella datumet och formatera det enligt önskat format
        String formattedDate = dateFormat.format(currentDate);
        
        // Skriv ut den formaterade datumet 
        System.out.println("Den aktuella datumet är: " + formattedDate);
    }
}
```
Sample output:
```
Den aktuella datumet är: 21/09/2021
```

## Djupdykning

Det är viktigt att notera att klassen `java.util.Date` inte bara innehåller information om datumet, utan även om tiden, såsom timmar, minuter och sekunder. Om du bara är intresserad av att hämta datumet bör du använda `java.sql.Date` istället, som endast innehåller information om datumet. Det är även möjligt att använda `java.util.Calendar` för att manipulera och utföra beräkningar med datum och tid.

## Se även

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) 
- [Java Calendar Class](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)