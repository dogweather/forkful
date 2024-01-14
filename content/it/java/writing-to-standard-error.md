---
title:                "Java: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché scrivere a standard error è importante

Scrivere a standard error è una parte essenziale della programmazione in Java. Sebbene in genere si utilizzi la funzione standard output per scrivere dati, a volte è necessario scrivere a standard error per catturare errori o problemi durante l'esecuzione del codice.

## Come scrivere a standard error in Java

Per scrivere a standard error in Java, è necessario utilizzare la classe `System` e il metodo `err` seguito dal metodo `println` o `printf` per stampare il contenuto desiderato. Ecco un esempio di codice:

```Java
// Importa la classe System nel tuo codice
import java.lang.System;

// Utilizza il metodo err per scrivere a standard error
System.err.println("Questo è un messaggio di errore.");
```

L'output di questo codice sarà:

```
Questo è un messaggio di errore.
```

Puoi anche utilizzare il metodo `printf` per formattare l'output a standard error. Ad esempio:

```Java
System.err.printf("Il risultato di %d + %d è %d", 1, 2, 3);
```

L'output sarà:

```
Il risultato di 1 + 2 è 3
```

## Approfondimento su scrivere a standard error in Java

Scrivere a standard error è utile quando si vogliono catturare errori o problemi durante l'esecuzione del codice. Inoltre, scrivere a standard error è un modo per differenziare i messaggi di errore dai normali output, rendendo più facile per gli sviluppatori individuare e risolvere eventuali problemi.

È importante notare che quando si scrive a standard error, il testo viene stampato immediatamente e senza alcun buffer, il che significa che i messaggi verranno visualizzati in ordine con cui sono stati emessi. Ciò può essere utile per il debugging, ma può anche rendere l'output meno leggibile.

## Vedi anche

- [Documentazione ufficiale di Java su System](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/System.html)
- [Articolo di Baeldung su System.out e System.err](https://www.baeldung.com/java-system-out-println-vs-system-err-println)
- [Esempi pratici di scrittura a standard error in Java](https://www.educba.com/java-system-err/)