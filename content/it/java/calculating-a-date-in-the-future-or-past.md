---
title:                "Java: Calcolare una data nel futuro o nel passato"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per molti scopi, come ad esempio per la gestione del tempo o per la pianificazione di eventi.

## Come Fare

Per calcolare facilmente una data nel futuro o nel passato in Java, possiamo utilizzare le funzionalità fornite dalla classe `Calendar`. Ad esempio, se vogliamo ottenere la data di 5 giorni fa, possiamo utilizzare il seguente codice:

```Java
Calendar cal = Calendar.getInstance();
cal.add(Calendar.DAY_OF_YEAR, -5);

System.out.println(cal.getTime());
```

Questo produrrà l'output seguente:

```
Mon Aug 16 03:54:10 GMT 2021
```

Possiamo anche specificare una data di partenza e utilizzare il metodo `add()` per aggiungere o sottrarre un determinato numero di giorni, mesi o anni.

Per esempio, se vogliamo calcolare la data di 2 mesi fa a partire da oggi, possiamo utilizzare il seguente codice:

```Java
Calendar cal = Calendar.getInstance();
cal.add(Calendar.MONTH, -2);

System.out.println(cal.getTime());
```

Questo produrrà l'output seguente:

```
Mon Jun 21 03:54:10 GMT 2021
```

## Approfondimento

La classe `Calendar` in Java è utilizzata per rappresentare una data e un orario specifici, ed è molto utile per gestire il calendario e le operazioni relative alle date. Possiamo anche utilizzare la classe `SimpleDateFormat` per formattare la data in un formato desiderato, come ad esempio `dd/MM/yyyy` per ottenere la data nel formato giorno/mese/anno.

È importante tenere presente che la classe `Calendar` utilizza il concetto di millisecondi come unità di tempo, quindi è necessario prestare attenzione quando si effettuano operazioni relative alla data per evitare errori.

## Vedi Anche

- [Java Calendar class documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java SimpleDateFormat class documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial su come manipolare le date in Java](https://www.baeldung.com/java-date-time-manipulation)