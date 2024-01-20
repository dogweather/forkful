---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer er prosessen med å fastslå om en dato er tidligere, senere, eller lik en annen dato. Programmerere gjør dette for å betjene funksjonaliteter som tidsfrister, planleggere, tidsstempling, og mye mer.

## Slik gjør du det:

Her er en enkel Java-kode for å sammenligne to datoer.

```Java
import java.time.LocalDate;

public class CompareDates {
   public static void main(String[] args) {

      LocalDate date1 = LocalDate.of(2022, 1, 1);
      LocalDate date2 = LocalDate.of(2022, 2, 1);

      if (date1.isAfter(date2)) {
         System.out.println(date1 + " comes after " + date2);
      } else if (date1.isBefore(date2)) {
         System.out.println(date1 + " comes before " + date2);
      } else {
         System.out.println(date1 + " is the same day as " + date2);
      }
   }
}
```

Når dette kjøres vil resultatet være:
```
2022-01-01 comes before 2022-02-01
```

## Dypdykk

Datoer har alltid vært en kritisk del av datasystemer, men håndteringen av dem har utviklet seg over tid. I tidligere Java-versjoner brukte vi `java.util.Date` og `java.util.Calendar` for å sammenligne datoer, men disse hadde mange problemer og inkonsistenser. 

I Java 8 introduserte de en ny date-time API, `java.time`, som inkluderer `LocalDate`. Denne koden er enklere, mer intuitiv, og løser mange problemer som oppstod med de gamle klassene. 

Alternativer til `LocalDate` er `LocalDateTime` (som også inkluderer tidspunkt på dagen) og `ZonedDateTime` (som også tar hensyn til tidssoner). 

## Se Også

Hvis du ønsker å dykke dypere inn i emnet, check ut følgende ressurser:

- [Oracle sin guide til `java.time`](https://docs.oracle.com/javase/tutorial/datetime)
- [Baeldung sin artikkel på sammenligning av datoer i Java](https://www.baeldung.com/java-comparing-dates)
- [Stack Overflow diskusjoner på sammenligning av datoer i Java](https://stackoverflow.com/questions/11093326/comparing-two-dates-in-java)