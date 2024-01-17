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

## Vad & Varför?
Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift som programmerare behöver hantera. Det kan vara användbart för att bestämma utgångsdatum för projekt eller att göra beräkningar baserat på ett specifikt datum.

## Så här gör du:
För att beräkna ett datum i Java, används Date-klassen och dess metoder. Här är ett exempel där vi räknar ut datumet som ligger 10 dagar efter ett givet startdatum:

```Java
import java.util.Date;

public class DateCalculator {

    public static void main(String[] args) {
        // Skapa ett startdatum
        Date startDate = new Date();

        // Använd Date-klassens .setTime-metod för att lägga till 10 dagar till startdatumet
        startDate.setTime(startDate.getTime() + (10 * 24 * 60 * 60 * 1000));

        // Skriv ut resultatet
        System.out.println("Datumet som ligger 10 dagar efter startdatumet är: " + startDate);
    }
}

/* Output:
Datumet som ligger 10 dagar efter startdatumet är: Mon Jul 19 14:33:35 CEST 2021
*/
```

## Djupdykning:
Att beräkna datum i framtiden eller det förflutna är en vanlig uppgift inom programmering, och detta har varit en viktig funktion sedan de tidiga dagarna av datorer. Alternative metoder för att beräkna datum i Java inkluderar att använda Calendar-klassen eller att använda tredjeparts bibliotek som Joda-Time. När det gäller implementationen av detta i Java, används "Unix timestamp" som är antalet millisekunder från 1 januari 1970.

## Se även:
- [Java Date-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Alternativ för datumberäkning i Java](https://stackify.com/how-to-get-current-date-time-in-java/)
- [Joda-Time biblioteket](https://www.joda.org/joda-time/)