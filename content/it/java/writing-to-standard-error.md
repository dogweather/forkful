---
title:                "Scrivere su errori standard"
html_title:           "Java: Scrivere su errori standard"
simple_title:         "Scrivere su errori standard"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché:
Scrivere su standard error in Java è un modo per visualizzare messaggi di errore o di debug sulla console dei programmi. È comunemente utilizzato dai programmatori per capire dove si verifica un errore o per monitorare il flusso di esecuzione del loro codice.

## Come fare:
Per scrivere su standard error in Java, è possibile utilizzare la classe System.err e il suo metodo println. Ecco un esempio di codice:

```Java
System.err.println("Messaggio di errore o di debug");
```

Questo codice stamperà il messaggio sulla console degli errori. Ecco un possibile output:

```
Messaggio di errore o di debug
```

## Approfondimento:
Scrivere su standard error è una pratica comune nelle linguaggi di programmazione. Ha origine dai vecchi sistemi Unix, dove standard error veniva utilizzato per visualizzare messaggi di errore. In Java, è considerato una buona pratica separare l'output dei messaggi di errore da quello dei messaggi di output regolari utilizzando System.out.

Un'alternativa all'utilizzo di System.err è l'utilizzo di un logger, come log4j o SLF4J. Questi strumenti hanno funzionalità più avanzate per la gestione dei messaggi di errore e di debug e possono essere configurati per scrivere in diversi tipi di output, come file di log o database.

Per implementare la scrittura su standard error in Java, è possibile utilizzare anche la classe PrintWriter e il suo costruttore con parametro OutputStream per scrivere su uno stream diverso, come un file di log.

## Vedi anche:
- [Documentazione Java su System.err](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html#err)
- [Documentazione Apache log4j](https://logging.apache.org/log4j/2.x/)
- [Documentazione SLF4J](http://www.slf4j.org/)