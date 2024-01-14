---
title:    "Java: Calcolare una data nel futuro o passato"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché

Calcolare le date nel passato o nel futuro è una funzionalità importante che può essere utile in molte situazioni, come pianificare eventi o lavorare con dati storici.

## Come fare

Per calcolare una data nel futuro o nel passato in Java, è possibile utilizzare la classe `Calendar`. Vediamo un esempio di codice che calcola la data di oggi più 7 giorni:

```Java
Calendar cal = Calendar.getInstance(); // Ottiene l'istanza corrente della classe Calendar
cal.add(Calendar.DATE, 7); // Aggiunge 7 giorni alla data di oggi
Date futureDate = cal.getTime(); // Ottiene la data calcolata come oggetto di tipo Date
```

In questo modo, è possibile anche calcolare date nel passato, semplicemente sostituendo il numero dei giorni da aggiungere con un numero negativo. Ad esempio, per ottenere la data di oggi meno un mese:

```Java
Calendar cal = Calendar.getInstance();
cal.add(Calendar.MONTH, -1);
Date pastDate = cal.getTime();
```

È inoltre possibile specificare la data di partenza utilizzando il metodo `set()`. Ad esempio, per calcolare la data di inizio dell'anno corrente:

```Java
Calendar cal = Calendar.getInstance();
cal.set(Calendar.MONTH, Calendar.JANUARY); // Imposta il mese a gennaio
cal.set(Calendar.DATE, 1); // Imposta il giorno al primo
Date newYear = cal.getTime();
```

## Approfondimento

La classe `Calendar` è uno strumento molto potente per lavorare con le date in Java. Per ulteriori informazioni sugli altri metodi disponibili, si consiglia di consultare la documentazione ufficiale di Java.

Inoltre, esistono anche altre librerie esterne che offrono funzionalità più avanzate per la gestione delle date, come ad esempio Joda Time o la libreria del pacchetto `java.time` introdotta in Java 8. È importante scegliere la soluzione più adatta alla propria esigenza.

## Vedi anche

- Documentazione ufficiale di Java per la classe `Calendar`: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Joda Time: https://www.joda.org/joda-time/
- Pacchetto `java.time` di Java 8: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html