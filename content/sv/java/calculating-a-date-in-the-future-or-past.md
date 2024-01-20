---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Java: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller förflutet är en process där vi skjuter framåt eller bakåt i tiden med en viss period. Programmerare gör detta för funktioner som påminnelser, tidslinjer, kalenderfunktioner och mer.

## Hur gör man:

Java tillhandahåller `java.time` biblioteket för datum och tidoperationer. Här är några exempel på hur du kan manipulera datum:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String args[]) {
        LocalDate idag = LocalDate.now();  // dagens datum
        System.out.println("Idag: " + idag);

        LocalDate framtiden = idag.plusDays(10);  // tio dagar in i framtiden
        System.out.println("Framtiden: " + framtiden);

        LocalDate forflutet = idag.minusWeeks(2);  // två veckor tillbaka
        System.out.println("Förfluten: " + forflutet);
    }
}
```

Provkörning producerar:

```Java
Idag: 2022-01-15
Framtiden: 2022-01-25
Förfluten: 2022-01-01
```

## Djupare dyk:

Historiskt sett var datum och tidsberäkningar svåra i Java innan java.time-paketet introducerades i Java 8. Tidigare användes `java.util.Date` och `java.util.Calendar`, men de hade brister som brist på trådsäkerhet och obekväma API:er. 

Alternativ till Java-standarden inkluderar Joda-Time-biblioteket, men sedan Java 8-förbättringarna rekommenderas java.time starkt.

Implementation av datumsberäkning i Java hanterar alla komplexiteter som skottår, olika månadslängder, tidszoner, etc.

## Se även:

1. Oracle Java Docs for java.time: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. Joda-Time library: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
3. Baeldung guide to date/time manipulation: [https://www.baeldung.com/java-8-date-time-intro](https://www.baeldung.com/java-8-date-time-intro)