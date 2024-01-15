---
title:                "Ottenere la data attuale"
html_title:           "Java: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore Java, probabilmente il tuo lavoro o il tuo hobby richiede di lavorare con le date. Conseguentemente, sapere come ottenere la data corrente è un'abilità essenziale per scrivere codice efficiente e preciso.

## Come fare
Per ottenere la data corrente in Java, esistono diverse opzioni. La più semplice è utilizzare la classe `LocalDate` del package `java.time`.

```Java
import java.time.LocalDate;

public class DateExample {
    public static void main(String[] args) {
        //Otteniamo la data corrente come oggetto LocalDate
        LocalDate currentDate = LocalDate.now();
        System.out.println("Data corrente: " + currentDate);
    }
}
```

L'output di questo codice sarà: `Data corrente: <anno>-<mese>-<giorno>`.

Se invece hai bisogno di ottenere la data e l'ora corrente, puoi utilizzare la classe `LocalDateTime` analogamente alla classe `LocalDate`.

```Java
import java.time.LocalDateTime;

public class DateTimeExample {
    public static void main(String[] args) {
        //Otteniamo la data e l'ora corrente come oggetto LocalDateTime
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println("Data e ora corrente: " + currentDateTime);
    }
}
```

L'output di questo codice sarà: `Data e ora corrente: <anno>-<mese>-<giorno>T<ora>:<minuti>:<secondi>.<millisecondi>`.

È possibile anche ottenere la data e l'ora corrente in una timezone specifica, specificando il fuso orario come parametro per i metodi `now()`.

```Java
import java.time.LocalDateTime;
import java.time.ZoneId;

public class TimeZoneExample {
    public static void main(String[] args) {
        //Otteniamo la data e l'ora corrente in un fuso orario specifico
        LocalDateTime currentDateTime = LocalDateTime.now(ZoneId.of("America/New_York"));
        System.out.println("Data e ora corrente a New York: " + currentDateTime);
    }
}
```

L'output di questo codice sarà: `Data e ora corrente a New York: <anno>-<mese>-<giorno>T<ora>:<minuti>:<secondi>.<millisecondi>`.

## Deep Dive
La classe `LocalDate` è una delle tante classi presenti nel nuovo package `java.time` introdotto nella versione 8 di Java. Questo package fornisce una maggiore precisione e flessibilità rispetto alle vecchie classi `Date` e `Calendar`.

Il metodo `now()` della classe `LocalDate` restituisce un oggetto che rappresenta la data corrente nel sistema locale in cui il programma è in esecuzione. Questo significa che ogni volta che il codice viene eseguito, sarà restituita la data e l'ora corrente del sistema in cui è in esecuzione, rendendo i risultati più accurati rispetto alle vecchie classi.

Puoi anche impostare manualmente una data utilizzando il metodo `of()` e fornendo il giorno, il mese e l'anno desiderati.

```Java
import java.time.LocalDate;

public class DateManipulation {
    public static void main(String[] args) {
        //Impostiamo manualmente la data al 1 gennaio 2021
        LocalDate date = LocalDate.of(2021, 1, 1);
        System.out.println("Data impostata manualmente: " + date);
    }
}
```

L'output di questo codice sarà: `Data impostata manualmente: 2021-01-01`.

Per ulteriori informazioni su come gestire le date e il tempo in Java, puoi esplorare il package `java.time` o consultare la documentazione ufficiale di Java.

## Vedi Anche
- [Documentazione ufficiale di Java su java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Tutorial su come gestire le date e il tempo in Java](https://www.baeldung.com/java-date-time)
- [Esempi pratici di utilizzo delle classi java.time](https://programming.guide/java/8-date-examples.html)