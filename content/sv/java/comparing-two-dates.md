---
title:                "Jämföring av två datum"
html_title:           "Java: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Det finns många anledningar till varför man kan vilja jämföra två datum i Java. Det kan vara för att kontrollera om ett datum är större, mindre eller lika med ett annat, eller för att jämföra hur lång tid som gått mellan två datum.

## Hur man gör
För att jämföra två datum i Java, används metoden `compareTo()` som finns tillgänglig i klassen `Date`. Den här metoden jämför råa värden från två datum och returnerar en negativ, noll eller positiv siffra beroende på om det första datumet är före, samma eller efter det andra datumet.

Här är ett exempel på hur man jämför två datum:
```Java
import java.util.Date;

public class CompareDates {

    public static void main(String[] args) {
        
        // Skapa två Date-objekt
        Date date1 = new Date(2020, 10, 15);
        Date date2 = new Date(2020, 5, 30);
        
        // Jämför de två datum
        int result = date1.compareTo(date2);
        
        // Skriv ut resultatet
        if (result < 0) {
            System.out.println("date1 är tidigare än date2");
        } else if (result > 0) {
            System.out.println("date2 är tidigare än date1");
        } else {
            System.out.println("date1 och date2 är samma datum");
        }
    }
}
```
Output:
```
date1 är efter date2
```
## Djupdykning
När man jämför datum i Java är det viktigt att förstå skillnaden mellan klasserna `Date`, `Calendar` och `LocalDate`. Klasserna `Date` och `Calendar` är äldre och har vissa begränsningar, som att de inte är trådsäkra och inte har stöd för dagligt sparande av sommartid. Därför rekommenderas det att använda den nya klassen `LocalDate` för att jämföra datum.

En annan viktig sak att tänka på när man jämför datum i Java är att det kan finnas skillnader i hur datum representeras på datorer med olika tidszoner. För att undvika förvirring bör man använda metoden `setTimeZone()` för att ställa in önskad tidszon innan man jämför datum.

## Se även
Här är några länkar till andra resurser som kan vara användbara när man jobbar med att jämföra datum i Java:
- Oracle Java dokumentation: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html#compareTo-java.util.Date-
- Javatpoint tutorial: https://www.javatpoint.com/java-date-compareto-method
- JavaWorld article: https://www.javaworld.com/article/2074803/core-java/when-to-use-compareto--equals---and-equalsignorecase-.html