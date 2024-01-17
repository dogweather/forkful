---
title:                "Convertire una data in una stringa"
html_title:           "Java: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una data in una stringa è un processo comune all'interno della programmazione Java. Consiste nell'utilizzo di metodi e strumenti per trasformare una data in un formato leggibile per gli utenti. I programmatori fanno ciò perché è utile per visualizzare date in diversi formati o per salvare le date in un formato specifico all'interno del codice.

## Come fare:
Per convertire una data in una stringa in Java, è possibile utilizzare il metodo ```format``` della classe ```SimpleDateFormat```. Questo metodo accetta due argomenti: il primo è la data da convertire e il secondo è il formato desiderato in cui si vuole visualizzare la data. Ecco un esempio di codice:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
  public static void main(String[] args) {
    Date today = new Date();
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    String todayString = dateFormat.format(today);
    System.out.println(todayString);
  }
}
```

L'output di questo codice sarà la data di oggi nel formato "gg/MM/aaaa". È possibile modificare il formato passando una stringa diversa al metodo ```SimpleDateFormat```, ad esempio "dd.MM.yyyy" o "MM-dd-yyyy".

## Analisi Approfondita:
Nel passato, convertire una data in una stringa era un processo più complicato e spesso richiedeva l'uso di librerie esterne. Tuttavia, con l'avvento di Java 8, sono stati introdotti i nuovi metodi nella classe ```LocalDateTime``` e ```DateTimeFormatter``` che rendono più semplice e intuitivo il processo di conversione.

Un'alternativa alla classe ```SimpleDateFormat``` è l'uso della classe ```DateTimeFormatter```, che offre una maggiore flessibilità nella formattazione delle date. Inoltre, è importante notare che il metodo ```format``` restituisce una stringa, pertanto se si desidera salvare una data in un formato specifico all'interno del codice, è necessario utilizzare il metodo ```parse``` per convertire la stringa in un oggetto di tipo ```Date```.

## Vedi anche:
- [Documentazione ufficiale di Oracle su SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentazione ufficiale di Oracle su LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Documentazione ufficiale di Oracle su DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)