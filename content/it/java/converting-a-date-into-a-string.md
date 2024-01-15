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

## Perché

Ci sono diverse situazioni in cui può essere necessario convertire una data in una stringa:
- Stampa di una data su un output o interfaccia utente
- Salvataggio di una data in un database o file di testo
- Manipolazione di date all'interno del codice

In queste situazioni, è importante conoscere come convertire correttamente una data in una stringa, per garantire la precisione e la correttezza delle informazioni gestite dal programma.

## Come Fare

Per convertire una data in una stringa in Java, è possibile utilizzare il metodo `format` della classe `SimpleDateFormat`. Questo metodo accetta due argomenti: il pattern di formato della data e la data da convertire. Ad esempio:

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
Date data = new Date();
String dataString = sdf.format(data);
System.out.println(dataString);
```

Questo codice produrrà l'output: `17/06/2021`, rappresentando la data odierna in formato stringa.

Il pattern di formato utilizzato nel metodo `SimpleDateFormat` segue alcune convenzioni. Ad esempio, `dd` rappresenta il giorno del mese con due cifre, mentre `MM` rappresenta il mese con due cifre. È possibile trovare una lista completa dei pattern disponibili nella documentazione ufficiale di Java.

## Approfondimento

La classe `SimpleDateFormat` è solo una delle molte opzioni disponibili per la conversione di date in stringhe. È possibile utilizzare anche altre classi, come `DateTimeFormatter` della libreria `java.time` introdotta in Java 8, per ottenere risultati più personalizzati e precisini.

Inoltre, è importante fare attenzione alle differenze tra le diverse regioni e lingue, poiché il formato della data può variare. Ad esempio, mentre in Italia si utilizza il formato `dd/MM/yyyy`, in altri paesi potrebbe essere utilizzato `yyyy/MM/dd`.

## Vedi Anche

- [Documentazione ufficiale di Java su SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Documentazione ufficiale di Java su DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutorial su come manipolare le date in Java](https://www.baeldung.com/java-date-time-manipulation)