---
title:                "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un aspetto fondamentale della programmazione in Java, in quanto fornisce informazioni utili per diverse attività, come ad esempio la gestione di eventi, l'ordinamento di dati e la creazione di log di sistema. Inoltre, è essenziale per garantire che un'applicazione funzioni correttamente e in modo sincronizzato con il resto del sistema.

## Come fare

Ci sono diverse opzioni per ottenere la data corrente in Java. Una delle più semplici è utilizzare la classe `LocalDate` del package `java.time`:

```Java
import java.time.LocalDate;

LocalDate dataCorrente = LocalDate.now();
System.out.println(dataCorrente);
```

Il codice sopra restituirà la data corrente nel formato standard ISO, ossia YYYY-MM-DD. È anche possibile formattarla in modo personalizzato utilizzando un oggetto `DateTimeFormatter`, ad esempio:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

LocalDate dataCorrente = LocalDate.now();
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/YYYY");
String dataFormattata = dataCorrente.format(formatter);
System.out.println(dataFormattata);
```

In questo caso, la data verrà stampata nel formato specificato, come ad esempio 13/01/2021.

Altre opzioni per ottenere la data corrente includono l'utilizzo delle classi `Date` e `Calendar`, che sono state introdotte prima della versione 8 di Java e sono ancora supportate per ragioni di compatibilità. Tuttavia, si consiglia di utilizzare le classi del package `java.time`, che sono più moderne e offrono una maggiore flessibilità e facilità d'uso.

## Approfondimento

Per comprendere meglio come ottenere la data corrente in Java, è importante capire il concetto di timezone (fuso orario). Quando si ottiene la data corrente, questa è sempre legata al fuso orario del sistema in cui è eseguita l'applicazione. Ad esempio, se si esegue il codice su un computer che si trova in Italia, la data corrente verrà restituita nel fuso orario GMT+1.

È possibile specificare manualmente il fuso orario utilizzando la classe `ZonedDateTime`. Inoltre, è importante considerare le possibili differenze tra i fusi orari di diversi sistemi in cui l'applicazione può essere eseguita.

## Vedi anche

- Documentazione ufficiale di Java sulla classe `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Guida completa ai fusi orari in Java: https://www.baeldung.com/java-time-zones