---
title:                "Scrivere su errore standard"
html_title:           "C: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un'azione importante quando si programma in C, in quanto consente di stampare messaggi di errore e di debug durante l'esecuzione del programma. In questo modo, è possibile identificare e risolvere eventuali problemi nel codice.

## Come

Per scrivere su standard error in C, è necessario utilizzare la funzione `fprintf()` e specificare `stderr` come primo argomento. Ad esempio:

```C
fprintf(stderr, "Questo è un messaggio di errore\n");
```

Questo codice stamperà il messaggio "Questo è un messaggio di errore" su standard error. È anche possibile specificare il formato del messaggio utilizzando gli stessi parametri della funzione `printf()`.

## Deep Dive

In C, ci sono tre stream standard predefiniti: `stdin`, `stdout` e `stderr`. `stdin` viene utilizzato per leggere input dall'utente, `stdout` viene utilizzato per stampare messaggi di output e `stderr` viene utilizzato per stampare messaggi di errore. Inoltre, `stderr` è solitamente associato al terminale, rendendolo un ottimo modo per visualizzare messaggi di errore durante l'esecuzione del programma.

È anche importante notare che `stderr` ha una priorità più alta rispetto a `stdout`, il che significa che i messaggi su `stderr` verranno visualizzati prima di quelli su `stdout`, anche se sono stati stampati prima.

## Vedi Anche

- [La funzione fprintf su C Reference](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Guida a errori e debug su C Programming](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
- [Stream e file standard su OpenGenus IQ](https://iq.opengenus.org/stream-standard-files-in-c/)