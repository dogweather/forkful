---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Java: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Calcolare una data futura o passata significa determinare una data specifica avanzando o tornando indietro da un certo numero di giorni, mesi o anni. I programmatori fanno questo per gestire la programmazione delle attività, calcolare la scadenza o tracciare gli eventi storici.

## Come fare:

Per calcolare una data futura o passata in Java, possiamo utilizzare la classe `java.time.LocalDate`, presente dal Java 8 in poi. Di seguito è presentato un esempio di calcolo di una data futura di 15 giorni.

```Java
import java.time.LocalDate;

public class AddDaysExample {
    public static void main(String[] args) {
        LocalDate oggi = LocalDate.now();
        System.out.println("Data di oggi: " + oggi);

        LocalDate futura = oggi.plusDays(15);
        System.out.println("Data futura: " + futura);
    }
}
```

E l'output sarà simile a:

```
Data di oggi: 2022-04-01
Data futura: 2022-04-16
```
  
## Approfondimento

Prima dell'introduzione delle classi `java.time` in Java 8, i programmatori solitamente usavano `java.util.Date` o `java.util.Calendar` per calcolare le date future o passate, che presentano tuttavia alcune limitazioni e problemi.

Oltre a `plusDays()`, `java.time.LocalDate` fornisce anche metodi come `plusWeeks()`, `plusMonths()`, `plusYears()` per aggiungere settimane, mesi o anni. Analogamente ci sono metodi `minusDays()`, `minusWeeks()`, `minusMonths()`, `minusYears()` per calcolare una data passata.

Per esempio, per determinare una data 5 anni prima, faremo così:

```Java
LocalDate passata = oggi.minusYears(5);
```

## Link utili

1. Documentazione ufficiale di Java su `java.time.LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
2. Tutorial di Oracle su Date Time API di Java 8: https://docs.oracle.com/javase/tutorial/datetime/iso/
3. Tutorial completo su `java.time`: https://www.baeldung.com/java-8-date-time-intro

Ricorda, la pratica rende perfetti, quindi continua ad esercitarti con questi concetti. Buona programmazione!