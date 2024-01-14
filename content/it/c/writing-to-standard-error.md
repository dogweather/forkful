---
title:    "C: Scrivere su standard error"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un aspetto importante della programmazione in C. Questo ci consente di gestire gli errori e di fornire informazioni utili agli utenti del nostro programma.

## Come Fare

Per scrivere su standard error in C, è necessario utilizzare la funzione `fprintf()` e passare `stderr` come primo parametro. Ad esempio:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Questo è un messaggio di errore\n");
    return 0;
}
```

Questo codice stamperà il messaggio di errore "Questo è un messaggio di errore" su standard error.

## Approfondimento

La differenza principale tra standard output e standard error è che il secondo è utilizzato per stampare messaggi di errore e di debug, mentre il primo è utilizzato per l'output normale del programma. Inoltre, standard error è uno stream non bufferizzato, il che significa che i messaggi vengono stampati immediatamente senza aspettare che venga chiuso il programma.

Inoltre, è possibile utilizzare la funzione `perror()` per stampare un messaggio di errore con una stringa rappresentante il tipo di errore. Ad esempio:

```C
#include <stdio.h>
#include <errno.h>

int main() {
    FILE* file = fopen("non_esiste.txt", "r");
    if (file == NULL) {
        perror("Errore nell'apertura del file");
        return 1;
    }
    return 0;
}
```

Questo codice stamperà il messaggio di errore "Errore nell'apertura del file: No such file or directory" su standard error.

## Vedi Anche

- [Funzioni di output in C](https://www.programiz.com/c-programming/c-input-output)
- [Gestione degli errori in C](https://www.geeksforgeeks.org/error-handling-c-programs/)