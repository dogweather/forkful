---
title:                "Java: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en viktig del av många Java-program. Det kan hjälpa till att avgöra vilket datum som kommer först eller om de två datum är samma. Det är också användbart för att skapa filtreringar och valideringsmekanismer.

## Hur man gör det
För att jämföra två datum kan du använda Java Date-klassen. För att göra det, använd metoden ``compareTo()``, som jämför två datum och returnerar en integer som indikerar om det första datumet är före, lika eller efter det andra datumet.

```
import java.util.Date;

public class DateComparisonExample {

  public static void main(String argv[]) throws Exception {
      
    Date firstDate = new Date(119, 5, 13); // Skapa första datumet (åååå, månad, dag)
    Date secondDate = new Date(119, 5, 14); // Skapa andra datumet (åååå, månad, dag)
         
    int result = firstDate.compareTo(secondDate); // Jämför första datumet med det andra datumet

    if (result < 0) {
        System.out.println("Första datumet är tidigare än det andra datumet.");
    } else if (result == 0) {
        System.out.println("Båda datum är samma.");
    } else {
        System.out.println("Första datumet är senare än det andra datumet.");
    }        
  }
}
```

Ovanstående kod visar hur du kan jämföra två datum och skriva ut resultatet baserat på det. I det här fallet kommer det att skriva ut "Första datumet är tidigare än det andra datumet".

## Djupdykning
För en mer detaljerad jämförelse av två datum kan Java API-metoden ``equals()`` användas. Detta gör en exakt jämförelse av både datum och tidszon. Det finns också andra metoder, till exempel ``before()``, ``after()`` och ``compareTo()``, som kan användas för att jämföra datum på olika sätt.

En viktig sak att tänka på när du jämför datum är tidszoner. Om du vill använda ett visst tidszonvärde för jämförelsen, måste du ange det i din kod.

## Se också
- [Java Date-klassens dokumentation](https://docs.oracle.com/javase/10/docs/api/java/util/Date.html)
- [Guide till Jämförelse och Ordning av Datum](https://www.java.com/en/download/help/compare_dates.xml)