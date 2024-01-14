---
title:                "Java: Ottenere la data corrente."
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Una delle funzionalità più utili nella programmazione è la capacità di ottenere la data corrente. Ciò può essere utile per registrare l'ora in cui è stato eseguito un programma, per generare report o semplicemente per visualizzare la data sullo schermo. In questo post, vedremo come ottenere la data corrente in Java.

## Come
In Java, per ottenere la data corrente, dobbiamo utilizzare la classe `Date` dal package `java.util`. Dopo aver importato la classe, possiamo istanziarla semplicemente con `new Date()`. Vediamo un esempio pratico:

```Java
import java.util.Date;

public class CurrentDate {
  public static void main(String[] args) {
    Date now = new Date(); // istanzia l'oggetto Date
    System.out.println(now); // stampa la data corrente
  }
}
```

L'esempio sopra restituirà l'output seguente: `Tue Nov 02 18:35:33 EST 2021`. Possiamo notare che la data è formattata in modo specifico. Possiamo modificare il formato utilizzando la classe `SimpleDateFormat`, specificando il pattern desiderato. Ad esempio, se volessimo visualizzare solo la data nel formato "dd/MM/yyyy":

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class CurrentDate {
  public static void main(String[] args) {
    Date now = new Date(); 
    SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
    System.out.println(sdf.format(now)); // output: 02/11/2021
  }
}
```

## Deep Dive
Ora, andiamo più in profondità e capiamo come funziona il processo di ottenere la data corrente. La classe `Date` rappresenta un'istante specifico nel tempo, misurato in millisecondi. Quando viene istanziata, viene impostata con la data e l'ora attuali del sistema. Ogni volta che viene rappresentata su uno schermo o stampata, viene utilizzato il fuso orario predefinito del sistema.

Per ottenere una data diversa dalla data corrente, possiamo utilizzare il costruttore `Date` che accetta un valore `long`, rappresentante il numero di millisecondi passati dal 1 gennaio 1970. Inoltre, possiamo utilizzare il metodo `setTime()` per impostare una data specifica a un oggetto `Date` esistente.

## Vedere anche
- [Java Date documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)