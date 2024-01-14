---
title:    "Java: Ottenere la data corrente"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La programmazione è una forma d'arte che può portare molte soddisfazioni a chi la pratica. Ma perché dovresti imparare a ottenere la data corrente in Java? Beh, è un'operazione fondamentale per la gestione del tempo nei tuoi programmi e ti consente di creare applicazioni più efficienti e precise.

## Come fare

Per ottenere la data corrente in Java, possiamo utilizzare la classe `LocalDate` dalla libreria `java.time`. Iniziamo importando queste librerie nel nostro codice:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
```

Adesso possiamo utilizzare il metodo `now()` della classe `LocalDate` per ottenere la data corrente nel formato desiderato. Ad esempio, se vogliamo visualizzare la data in formato "gg/mm/aaaa", possiamo usare il seguente codice:

```Java
LocalDate dataCorrente = LocalDate.now();
DateTimeFormatter formato = DateTimeFormatter.ofPattern("dd/MM/yyyy");
System.out.println("La data corrente è: " + dataCorrente.format(formato));
```

L'output di questo codice sarà simile a questo: `La data corrente è: 22/07/2021`.

## Approfondimento

Oltre a ottenere la data corrente, la classe `LocalDate` ci permette anche di manipolare facilmente le date. Ad esempio, possiamo aggiungere o sottrarre giorni, mesi o anni alla data corrente utilizzando i metodi `plusDays()`, `plusMonths()` e `plusYears()`. Possiamo anche confrontare due date utilizzando i metodi `isBefore()` o `isAfter()`. Per ulteriori informazioni su come gestire le date in Java, puoi consultare la documentazione ufficiale della classe `LocalDate`.

## Vedi anche

- Documentazione ufficiale di `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Tutorial su Java Date and Time API: https://www.baeldung.com/java-datetime-api
- Guida completa su come gestire le date in Java: https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant