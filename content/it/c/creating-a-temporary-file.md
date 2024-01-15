---
title:                "Creare un file temporaneo"
html_title:           "C: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'operazione comune nella programmazione in C ed è utile per diversi motivi. Ad esempio, può essere utilizzato per salvare i dati temporaneamente mentre un programma è in esecuzione o per creare file di log.

## Come fare

Per creare un file temporaneo in C, è necessario includere la libreria `stdlib.h`, che contiene le funzioni necessarie. Quindi, è possibile utilizzare le funzioni `tmpfile()` o `mkstemp()` per creare il file temporaneo.

```
#include <stdlib.h>

// Utilizzo della funzione tmpfile()
FILE *tmpfile();
```

```
#include <stdlib.h>

// Utilizzo della funzione mkstemp()
int fd = mkstemp("/tmp/tempfile-XXXXXX");
```

Entrambe le funzioni creeranno un file temporaneo in una posizione specifica, restituendo un puntatore a un file `FILE` o un file descriptor, rispettivamente. Si consiglia inoltre di utilizzare la funzione `fopen()` per aprire il file temporaneo per la scrittura.

```
FILE *fp = fopen("/tmp/tempfile", "w");
```

Una volta terminata l'esecuzione del programma, è importante eliminare il file temporaneo utilizzando la funzione `remove()`.

```
int remove(const char *filename);
```

Il codice completo per creare e scrivere in un file temporaneo potrebbe apparire così:

```
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Creazione del file temporaneo e apertura in scrittura
    FILE *fp = fopen("/tmp/tempfile", "w");

    // Controllo se il file è stato creato correttamente
    if (fp == NULL) {
        perror("Errore nella creazione del file");
        exit(EXIT_FAILURE);
    }

    // Scrittura di una stringa nel file
    fprintf(fp, "Questo è un file temporaneo!\n");

    // Chiusura del file
    fclose(fp);

    // Eliminazione del file temporaneo
    int result = remove("/tmp/tempfile");
    if (result == -1) {
        perror("Errore nella rimozione del file");
        exit(EXIT_FAILURE);
    }

    return EXIT_SUCCESS;
}
```

L'esecuzione di questo programma dovrebbe creare e scrivere nel file temporaneo, quindi eliminarlo alla fine.

## Approfondimento

La creazione di un file temporaneo può essere una pratica molto utile, ma è importante assicurarsi che il file venga rimosso correttamente alla fine dell'esecuzione del programma. In caso contrario, questo potrebbe causare problemi di spazio su disco o di sicurezza nel sistema.

Inoltre, è importante tenere presente che, essendo un file temporaneo, il suo contenuto potrebbe essere perso se il programma viene interrotto in modo anomalo. Per evitare ciò, si consiglia di scrivere periodicamente nel file temporaneo e di utilizzare un sistema di gestione dei segnali per assicurarsi che venga eliminato anche in caso di crash del programma.

## Vedi anche

- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Funzioni per la gestione dei file in C](http://www.cplusplus.com/reference/cstdio/)