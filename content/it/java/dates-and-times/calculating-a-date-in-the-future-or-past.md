---
title:                "Calcolo di una data futura o passata"
aliases:
- /it/java/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:21.029495-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Calcolare una data nel futuro o nel passato significa determinare un giorno specifico prima o dopo una data conosciuta. I programmatori lo fanno per gestire scadenze, appuntamenti o eventi pianificati.

## Come fare:
Per eseguire il calcolo di date in Java, si usa la classe `LocalDate` del package `java.time`. Ecco un esempio:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {

    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate tenDaysLater = today.plusDays(10);
        LocalDate threeWeeksEarlier = today.minusWeeks(3);
        
        System.out.println("Oggi: " + today);
        System.out.println("Tra 10 giorni: " + tenDaysLater);
        System.out.println("3 settimane fa: " + threeWeeksEarlier);
    }
}
```

Output esemplificativo:

```
Oggi: 2023-04-05
Tra 10 giorni: 2023-04-15
3 settimane fa: 2023-03-15
```

## Approfondimenti:
Il calcolo delle date nel futuro o nel passato ha basi storiche: pensate alle civiltà che prevedevano eventi astronomici. In Java, la gestione delle date si è evoluta da `java.util.Date` a `java.util.Calendar`, fino alle classi del package `java.time` introdotte in Java 8 con l'obiettivo di risolvere problemi di immutabilità e migliorare l'API.

Alternative prima di Java 8 includevano librerie di terze parti come Joda-Time, mentre ora l'API `java.time` è la scelta standard. Dettagli di implementazione, come i metodi `plus()` e `minus()`, supportano operazioni fluide con unità di tempo come giorni, mesi o anni, e molte altre funzionalità avanzate come la gestione di fusi orari con `ZonedDateTime`.

## Vedi Anche:
- La documentazione ufficiale di Oracle per `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Guida Java Date and Time di Baeldung: https://www.baeldung.com/java-8-date-time-intro
- Joda-Time, una valida alternativa prima di Java 8: https://www.joda.org/joda-time/
