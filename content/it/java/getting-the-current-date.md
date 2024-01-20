---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?
Ottenere la data corrente in Java significa avere accesso all'informazione esatta sul giorno, mese, anno, ore, minuti, secondi nel momento esatto in cui il codice viene eseguito. I programmatori lo fanno per diverse ragioni, come tracciare il tempo di esecuzione, creare timestamp o gestire attività legate ai dati temporali.

## Come Fare:
```Java
import java.time.LocalDateTime;
public class Main {
    public static void main(String[] args) {
        // Il metodo now() di LocalDateTime restituisce la data e l'ora correnti
        LocalDateTime current = LocalDateTime.now();

        System.out.println("Data e ora correnti: " + current);
    }
}
```
Quando esegui questo codice, avrai un output simile a questo:
```Java
Data e ora correnti: 2023-01-31T14:28:32.637370433
```
## Approfondimento
Java ha introdotto nuove classi per la gestione del tempo in Java 8. Prima di questa versione, si usava `java.util.Date` o `java.util.Calendar` per ottenere la data corrente, ma erano pieni di problemi. Le nuove classi, come `LocalDateTime`, sono molto più potenti e flessibili.

Esistono altre alternative per ottenere la data corrente in Java. Ad esempio, puoi usare `LocalDate.now()` per ottenere solo la data senza l'ora, o `ZonedDateTime.now()` per ottenere la data e l'ora correnti considerando il fuso orario.

La realizzazione di questa funzionalità è piuttosto semplice: `LocalDateTime.now()` internamente usa l'orologio del sistema per ottenere il tempo corrente, quindi lo incapsula in un oggetto `LocalDateTime`.

## Vedi Anche
1. Documentazione ufficiale Oracle per LocalDateTime: [here](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
2. Altri modi per ottenere la data corrente in Java: [here](https://www.baeldung.com/java-8-date-time-intro)
3. Tutorial su Java LocalDateTime: [here](https://www.javatpoint.com/java-localdatetime-class)