---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# La Lettura di un File di Testo in Linguaggio C

## Che Cosa & Perché?

Leggere un file di testo in linguaggio C significa accedere e interpretare i dati presenti in un file come stringhe di testo. Questo è fondamentale per poter manipolare, analizzare e utilizzare tali dati all'interno dei nostri programmi.

## Come Fare:

Ecco un esempio di come leggere un file di testo in C.

```C
#include <stdio.h>

int main() {
    FILE *file = fopen("file_di_testo.txt", "r");
    char stringa[256];

    if (file == NULL) {
        printf("Non posso aprire il file.\n");
        return 1;
    }
    
    while (fgets(stringa, sizeof(stringa), file)) {
        printf("%s", stringa);
    }

    fclose(file);
    return 0;
}
```

Se il file "file_di_testo.txt" contiene "Ciao, Mondo!", l'output del programma sarà:

```
Ciao, Mondo!
```

## Approfondimento

1. Contesto storico: Il C, sviluppato nei primi anni '70, ha sempre supportato la lettura di file di testo come operazione di base. 

2. Alternative: Ci sono molte altre funzioni in C per leggere un file di testo, come fscanf() o getc(). Ogni funzione ha i suoi pro e contro, a seconda delle esigenze specifiche del programma.

3. Dettagli di implementazione: Nel codice di esempio, usiamo 'fopen' per aprire il file, 'fgets' per leggere ogni riga e 'printf' per stamparla. Infine, 'fclose' viene utilizzata per chiudere il file dopo che siamo finiti di leggerlo. Ricorda, è sempre una buona abitudine chiudere i file dopo l'uso.

## Vedi Anche

- Documentazione di GNU C Library: https://www.gnu.org/software/libc/manual/html_node/I_002fO-Primitives.html 
- Tutorial su File I/O in C: https://www.learn-c.org/en/File_IO
- Documentazione di C Standard Library: http://www.cplusplus.com/reference/clibrary/ 

Ricordati, diventa un maestro nella lettura di file di testo! È uno strumento fondamentale per un programmatore C.