---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
Confrontare due date in Java significa verificare se una data è prima, dopo o uguale a un'altra. Questo è fondamentale in programmazione per manipolare dati temporali, organizzare eventi, pianificare compiti e così via.

## Come Fare:
Ecco un esempio sull'uso del metodo `compareTo` della classe `LocalDate`. Guarda il codice:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate data1 = LocalDate.of(2000, 11, 30);
        LocalDate data2 = LocalDate.of(1999, 11, 30);
      
        if (data1.compareTo(data2) > 0) {
            System.out.println("La data1 è successiva alla data2");
        } else if (data1.compareTo(data2) < 0) {
            System.out.println("La data1 è precedente alla data2");
        } else {
            System.out.println("La data1 è uguale alla data2");
        }
    }
}
```
Se eseguiamo questo codice l'output sarà `La data1 è successiva alla data2`.

## Approfondimenti
(1) Nel contesto storico, Java 1.0 utilizzava `java.util.Date` per rappresentare data e orario, che era ampiamente criticato per la sua scarsa progettazione. A partire da Java 8, è stato introdotto il package `java.time`, dando un modo migliore per confrontare le date.

(2) Come alternativa, possiamo anche usare i metodi `isAfter`, `isBefore` e `isEqual` di `LocalDate` per confrontare due date.

(3) Importante da considerare è che le date in Java sono immutabili. Ciò significa che qualsiasi operazione su una data restituisce una nuova istanza, mantenendo la vecchia intatta.

## Vedi Anche
- Documentazione ufficiale della classe LocalDate, per una maggiore comprensione dei metodi per confrontare le date: [`Oracle Docs`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- Tutorial approfondito sulla manipolazione delle date in Java: [`Baeldung Article`](https://www.baeldung.com/java-8-date-time-intro)