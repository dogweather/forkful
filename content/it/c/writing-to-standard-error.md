---
title:                "C: Scrivere su errore standard"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error in programmazione C può sembrare un'operazione inutile o poco utilizzata, ma in realtà è importante per la gestione degli errori e la risoluzione dei bug nel codice. Inoltre, può aiutare a visualizzare messaggi di debug durante lo sviluppo.

## Come fare

Per scrivere su standard error in C, è necessario includere la libreria `stdio.h` nel codice e utilizzare la funzione `fprintf` con il valore `stderr` come argomento. Ad esempio:

```C
#include <stdio.h>

int main(void) {
    fprintf(stderr, "Questo è un messaggio scritto su standard error\n");
    return 0;
}
```

L'output sarà mostrato sulla console standard di errore come:

```bash
Questo è un messaggio scritto su standard error
```

In questo modo, è possibile inviare messaggi di errore o di debug direttamente sulla console senza influenzare l'output standard del programma. Si consiglia di utilizzare questo metodo quando si vuole distinguere i messaggi di errore dai messaggi di output regolari.

## Approfondimento

La funzione `fprintf` ha la seguente sintassi:

```
int fprintf(FILE *stream, const char *format, ...)
```

Il primo argomento `stream` indica il file su cui scrivere, mentre il secondo argomento `format` è una stringa di formattazione che definisce il tipo di dati e il numero di argomenti successivi. Ci sono diversi tipi di dati che possono essere utilizzati, come `%s` per una stringa, `%d` per un intero e `%f` per un numero decimale. Ad esempio, `fprintf` può essere utilizzata per scrivere su file o eventualmente su altri tipi di stream diversi da `stderr`.

## Vedi anche

- [Manuale di riferimento di fprintf](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Cos'è e come usare standard error in C](https://www.geeksforgeeks.org/what-is-standard-error-in-c/)
- [Come gestire gli errori in C](https://www.programmingsimplified.com/c/error-handling-c-programs)