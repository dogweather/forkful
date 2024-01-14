---
title:    "Java: Ottenerela data corrente"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché
La data è una parte importante della nostra vita quotidiana. Sappiamo sempre il giorno e il mese in cui ci troviamo, ma a volte potremmo aver bisogno di ulteriori informazioni come il giorno della settimana o l'ora esatta. In questo post, impareremo come ottenere la data corrente utilizzando il linguaggio di programmazione Java.

## Come fare
Per ottenere la data corrente in Java, possiamo utilizzare la classe `LocalDate` dalla libreria `java.time`. Ecco un esempio di codice che stampa la data corrente:

```Java
import java.time.LocalDate;

public class DateExample {
    public static void main(String[] args) {
        //ottieni la data corrente
        LocalDate currentDate = LocalDate.now();

        //stampa la data in formato dd/mm/yyyy
        System.out.println("La data corrente è: " + currentDate.getDayOfMonth() + "/" + currentDate.getMonthValue() + "/" + currentDate.getYear());
    }
}
```

L'output di questo codice sarà: `La data corrente è: 14/09/2021`.

Possiamo anche ottenere la data corrente con il fuso orario specifico utilizzando la classe `ZonedDateTime` come mostrato nell'esempio seguente:

```Java
import java.time.ZonedDateTime;
import java.time.ZoneId;

public class DateExample {
    public static void main(String[] args) {
        //ottieni la data corrente con il fuso orario di Roma
        ZonedDateTime currentDate = ZonedDateTime.now(ZoneId.of("Europe/Rome"));

        //stampa la data in formato dd/mm/yyyy hh:mm:ss
        System.out.println("La data corrente è: " +
                currentDate.getDayOfMonth() + "/" +
                currentDate.getMonthValue() + "/" +
                currentDate.getYear() + " " +
                currentDate.getHour() + ":" +
                currentDate.getMinute() + ":" +
                currentDate.getSecond());
    }
}
```

L'output di questo codice sarà: `La data corrente è: 14/09/2021 18:30:12`.

## Approfondimenti
Per ottenere una migliore comprensione di come funziona il codice sopra riportato, è importante conoscere alcune nozioni di base sulla data e sull'ora in Java. Il pacchetto `java.time` è stato introdotto nella versione 8 di Java ed è il modo consigliato per gestire la data e l'ora. Include molte classi utili come `LocalDate`, `LocalTime`, `LocalDateTime`, `ZonedDateTime`, ecc. Ognuna di queste classi offre metodi specifici per ottenere parti specifiche della data o dell'ora.

Inoltre, è possibile giocare con i formati della data e dell'ora utilizzando la classe `DateTimeFormatter`. Questa classe ci consente di cambiare il formato predefinito della data e dell'ora e di convertirli in altri formati come ISO, RFC, ecc.

## Vedi anche
- [Documentazione ufficiale di Java sul pacchetto java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Un tutorial su come utilizzare il pacchetto java.time](https://www.baeldung.com/java-8-date-time-intro)