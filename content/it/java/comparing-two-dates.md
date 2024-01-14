---
title:                "Java: Confrontare due date"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con dati temporali, può essere necessario confrontare due date per diversi motivi. Ad esempio, per verificare se una data è successiva o precedente all'altra, oppure per calcolare la differenza di tempo tra le due date. In questo articolo, esploreremo come confrontare due date in Java.

## Come fare

In Java, le date sono gestite tramite la classe `Date` o `LocalDate` del package `java.util`. Per confrontare due date, dobbiamo prima convertirle in oggetti di una di queste classi.

Ecco un esempio di come fare con `Date`:

```Java
import java.util.Date;

public class DateComparison {
    public static void main(String[] args) {
        // Definiamo le due date da confrontare
        Date date1 = new Date(121, 3, 1); // 1 Aprile 2021
        Date date2 = new Date(121, 3, 15); // 15 Aprile 2021

        // Confrontiamo le date usando il metodo compareTo()
        int result = date1.compareTo(date2);

        if (result < 0) {
            System.out.println("La prima data è precedente alla seconda");
        } else if (result > 0) {
            System.out.println("La prima data è successiva alla seconda");
        } else {
            System.out.println("Le due date sono uguali");
        }
    }
}
```

Il risultato di questo programma sarà "La prima data è precedente alla seconda", in quanto 1 Aprile 2021 è una data precedente a 15 Aprile 2021.

Se invece vogliamo confrontare le date utilizzando la classe `LocalDate`, possiamo fare così:

```Java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        // Definiamo le due date da confrontare
        LocalDate date1 = LocalDate.of(2021, 4, 1); // 1 Aprile 2021
        LocalDate date2 = LocalDate.of(2021, 4, 15); // 15 Aprile 2021

        // Confrontiamo le date usando il metodo compareTo()
        int result = date1.compareTo(date2);

        if (result < 0) {
            System.out.println("La prima data è precedente alla seconda");
        } else if (result > 0) {
            System.out.println("La prima data è successiva alla seconda");
        } else {
            System.out.println("Le due date sono uguali");
        }
    }
}
```

In questo caso, il risultato sarà lo stesso dell'esempio precedente.

## Approfondimento

Per confrontare due date in maniera più precisa, possiamo utilizzare i metodi `before()` e `after()` della classe `Calendar` nel package `java.util`.

```Java
import java.util.Calendar;

public class DateComparison {
    public static void main(String[] args) {
        // Definiamo le due date da confrontare
        Calendar date1 = Calendar.getInstance();
        date1.set(2021, 3, 1); // 1 Aprile 2021
        Calendar date2 = Calendar.getInstance();
        date2.set(2021, 3, 15); // 15 Aprile 2021

        // Confrontiamo le date utilizzando i metodi before() e after()
        if (date1.before(date2)) {
            System.out.println("La prima data è precedente alla seconda");
        } else if (date1.after(date2)) {
            System.out.println("La prima data è successiva alla seconda");
        } else {
            System.out.println("Le due date sono uguali");
        }
    }
}
```

In questo caso, il risultato sarà nuovamente "La prima data è precedente alla seconda".

## Vedi anche

- [Java - Date Class](https://www.w3schools.com/java/java_date.asp)
- [Java - LocalDate Class](https://www.w3schools.com/java/java_localdate.asp)
- [Java - Calendar Class](https://www.w3schools.com/java/java_calendar.asp)