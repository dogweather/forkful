---
title:    "Java: Sammenligning av to datoer"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor
Sammenligning av to datoer er en vanlig oppgave i Java-programmering. Dette kan være nyttig når du trenger å validere datoer, sortere dem eller beregne tidsintervaller. Ved å lære hvordan du sammenligner to datoer i Java, vil du kunne håndtere datoer på en effektiv måte og unngå feil.

## Hvordan gjøre det
For å sammenligne to datoer i Java, kan du bruke klassen `LocalDate` og metoden `compareTo()`. La oss se på et eksempel på hvordan vi kan sammenligne to datoer og få ut en enkel beskjed avhengig av resultatet:

```Java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        // Lage to LocalDate objekter
        LocalDate firstDate = LocalDate.of(2021, 1, 1);
        LocalDate secondDate = LocalDate.of(2021, 1, 5);

        // Sammenligne datoene og få resultatet
        int result = firstDate.compareTo(secondDate);

        // Skrive ut beskjed basert på resultatet
        if (result == 0) {
            System.out.println("Datoene er like.");
        } else if (result < 0) {
            System.out.println("Den første datoen kommer før den andre datoen.");
        } else {
            System.out.println("Den første datoen kommer etter den andre datoen.");
        }
    }
}
```

Output:

```
Den første datoen kommer før den andre datoen.
```

Her kan vi se at metoden `compareTo()` returnerer en verdi som indikerer forholdet mellom de to datoene. Hvis verdien er 0, betyr det at datoene er like. Hvis verdien er mindre enn 0, betyr det at den første datoen kommer før den andre datoen, og hvis verdien er større enn 0, betyr det at den første datoen kommer etter den andre datoen.

## Dypdykk
Det er viktig å merke seg at når vi sammenligner to datoer, sammenlignes de basert på deres kronologiske rekkefølge, og tar hensyn til både år, måned og dag. Hvis vi for eksempel sammenligner 2021-01-01 og 2020-12-31, vil den første datoen bli ansett som større fordi den kommer etter i tid, til tross for at året er lavere.

Du kan også bruke andre metoder for å sammenligne datoer, som for eksempel `isBefore()` og `isAfter()`. Disse metodene vil returnere en boolsk verdi som indikerer om den ene datoen kommer før eller etter den andre datoen.

## Se også
- [Official Java Documentation for LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorialspoint - Java LocalDate](https://www.tutorialspoint.com/java8/java8_localdate.htm)
- [Baeldung - Comparing Dates in Java](https://www.baeldung.com/java-comparing-dates)