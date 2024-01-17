---
title:                "Ricerca e sostituzione di testo"
html_title:           "Java: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La ricerca e la sostituzione del testo sono tecniche comuni utilizzate dai programmatori per trovare e sostituire una determinata stringa di caratteri all'interno di un testo o di un codice. Ciò consente di automatizzare l'aggiornamento di parti del codice o di correggere eventuali errori.

## Come fare:
Di seguito è riportato un esempio di codice in Java che illustra come effettuare una ricerca e sostituzione del testo utilizzando il metodo `.replaceAll()`:

```
String testo = "Questo è un esempio di una frase con alcune parole ripetute."; // testo da analizzare
String sostituzione = "cane"; // stringa da sostituire
String nuovaFrase = testo.replaceAll("alcune parole ripetute", sostituzione); // esegue la sostituzione del testo
System.out.println(nuovaFrase); // output: Questo è un esempio di una frase con cane.
```

## Approfondimenti:
La ricerca e la sostituzione del testo sono diventate comuni nella programmazione moderna grazie alla loro capacità di risparmiare tempo e di facilitare la gestione del codice. In passato, i programmatori dovevano effettuare queste operazioni manualmente, ma grazie alle moderne tecnologie informatiche, è possibile automatizzarle. Esistono anche altre metodologie per effettuare la ricerca e la sostituzione del testo, come ad esempio l'utilizzo di espressioni regolari.

## Vedi anche:
- [Java String replaceAll()](https://www.javatpoint.com/java-string-replaceall)
- [Espressioni regolari in Java](http://docs.oracle.com/javase/tutorial/essential/regex/)