---
title:    "Java: Scrivere su errore standard"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sullo standard error è utile quando si vogliono visualizzare messaggi di errore o avvisi durante l'esecuzione di un programma. Invece di interrompere il flusso di output normale, i messaggi verranno visualizzati sullo standard error, rendendo più semplice la risoluzione dei problemi e la comprensione dei malfunzionamenti.

## Come fare

Per scrivere su standard error in Java, è necessario utilizzare il metodo `System.err.println()`, che accetta come argomento la stringa da stampare. Ad esempio:

```Java
System.err.println("Errore: valore inserito non valido");
```

Questo codice stamperà la stringa "Errore: valore inserito non valido" su standard error. Ecco un esempio di output:

```
Errore: valore inserito non valido
```

## Approfondimento

Lo standard error, o stderr, è un'area della memoria utilizzata per scrivere messaggi di errore e avvisi durante l'esecuzione di un programma. A differenza dello standard output, o stdout, che viene visualizzato sullo schermo, lo stderr viene di solito reindirizzato in un file di log o visualizzato solo in caso di errori. Scrivere su stderr è un modo utile per gestire i messaggi di errore all'interno del codice e rendere più efficiente la risoluzione dei problemi.

## Vedi anche

- [Documentazione Java su System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Guida su come gestire gli errori in Java](https://www.baeldung.com/java-error-handling)
- [Accedere a standard input, output e error in Java](https://www.geeksforgeeks.org/access-sdterr-stdoutr-and-stdin-in-java/)