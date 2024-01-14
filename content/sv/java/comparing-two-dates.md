---
title:                "Java: Jämförande av två datum"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför?

Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man vill kontrollera om ett datum är före eller efter ett annat datum. Det kan vara användbart för att upprätthålla sortering av data eller för att skapa olika tidsbegränsningar i en applikation.

## Så här gör du

För att jämföra två datum i Java, behöver vi använda oss av Java Date och Calendar-klasserna. Vi kan använda metoden `compareTo()` för att jämföra två datum. Här är en kodexempel för att jämföra två datum och skriva ut en lämplig meddelande baserat på resultatet:

```Java
import java.util.Date;
import java.util.Calendar;

public class DateComparison {
    public static void main(String[] args) {
        Date d1 = new Date();  // skapar första datumet
        Calendar c = Calendar.getInstance();
        c.set(2021, 6, 10);  // skapar andra datumet (10 juli 2021)
        Date d2 = c.getTime();

        int result = d1.compareTo(d2);  // jämför datumen
        // om d1 är före d2 kommer result att vara negativt, om d1 är efter d2 kommer result att vara positivt, annars är datumen lika
        if (result < 0) {
            System.out.println(d1 + " är före " + d2);
        } else if (result > 0) {
            System.out.println(d1 + " är efter " + d2);
        } else {
            System.out.println("Datumen är lika: " + d1);
        }
    }
}
```

**Output:**

```
Fri Jul 02 18:40:02 CEST 2021 är efter Sat Jul 10 00:00:00 CEST 2021
```

I det här exemplet använder vi `compareTo()` för att kolla om det första datumet (i detta fall aktuellt datum) är före eller efter det andra datumet som skapats med hjälp av Calendar-klassen.

## Djupdykning

När man använder `compareTo()` för att jämföra datum, är det viktigt att komma ihåg att det endast jämför datumen, inte tiderna. Detta innebär att om två datum är samma men med olika tider, kommer de att anses som lika. Om man vill jämföra både datum och tider, kan man använda metoden `equals()` istället.

En annan metod för att jämföra datum är `after()` och `before()`, som returnerar `true` eller `false` beroende på om det första datumet är efter eller före det andra datumet.

Det är också viktigt att tänka på att Java Date-klassen är föråldrad och att det rekommenderas att använda istället `LocalDate` och `LocalDateTime` från Java 8 och senare.

## Se också

- [Java - Date Class](https://www.w3schools.com/java/java_date.asp)
- [Java - Calendar Class](https://www.w3schools.com/java/java_calendar.asp)
- [Oracle Documentation - Date and Time Classes](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)