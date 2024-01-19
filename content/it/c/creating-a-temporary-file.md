---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Creare un file temporaneo in C significa creare un file che serve solo per un breve periodo di tempo, solitamente per conservare dati temporanei durante l'esecuzione di un programma. Gli sviluppatori lo fanno spesso per salvare lo stato del programma, evitare di dover ricominciare da capo in caso di crash, o fare alcune operazioni su file senza alterare i file originali.

## Come fare:

Ecco un esempio del codice per fare un file temporaneo usando la funzione `tmpfile()`:

```C
#include <stdio.h>

int main() {
    FILE* temp = tmpfile();

    if(temp == NULL) {
        perror("Impossibile creare il file temporaneo.");
        return 1;
    }

    fputs("Salute, mondo!\n", temp);

    rewind(temp);

    char buffer[16];
    fgets(buffer, sizeof buffer, temp);

    printf("Lettura dal file temporaneo: '%s'.\n", buffer);

    return 0;
}
```

In questo esempio, creo un file temporaneo, ci scrivo una breve string ("Salute, mondo!\n"), poi torno all'inizio del file (usando `rewind()`), leggo ciò che ho appena scritto e lo stampo. Quando il programma esce, il file temporaneo viene automaticamente eliminato.

## Approfondimento

Creare file temporanei era una pratica comune nei primi giorni della programmazione, quando la RAM era limitata e l'uso di file temporanei era un modo efficace per gestire grandi quantità di dati all'interno di un'applicazione.  
Fra le alternative a `tmpfile()` ci sono `tmpnam()` e `mktemp()`, che però sono considerate insicure a causa di potenziali condizioni di race. Se l'unico bisogno è di uno spazio temporaneo per manipolare dati, invece di un file, considera l'uso di memoria dinamica (`malloc()`, `calloc()`, e simili).  
`tmpfile()` funziona creando un file unico in una directory adatta per i file temporanei, aprendolo in lettura+scrittura ("w+b"), e poi rimuovendo il file così che desaparece non appena è chiuso.

## Vedi anche:

Il problema di creare un file temporaneo è ben documentato online. Ecco alcuni link utili:

- La pagina del manuale Unix su `tmpfile()`: https://linux.die.net/man/3/tmpfile
- Un'esplorazione più profonda del problema dei file temporanei sul blog di OpenBSD: https://www.tedunangst.com/flak/post/-tmp-
- Le funzioni di gestione della memoria dinamica in C tutorial - GeeksforGeeks: https://www.geeksforgeeks.org/dynamic-memory-allocation-in-c-using-malloc-calloc-free-and-realloc/