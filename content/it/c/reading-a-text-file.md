---
title:                "Leggere un file di testo"
html_title:           "C: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività più comuni in programmazione, soprattutto quando si lavora con input proveniente dall'utente o da altri sistemi. Conoscere come leggere un file di testo in C può aiutare a gestire meglio i dati e rendere il codice più efficiente.

## Come

Per leggere un file di testo in C, è necessario utilizzare alcune funzioni della libreria standard, come `fopen`, `fread` e `fclose`. Ecco un esempio di come aprire un file di testo in modalità di lettura e leggerne il contenuto:

```C
#include <stdio.h>

int main() {
    // Dichiarazione di una variabile per il file
    FILE *fp;
    // Apertura del file in modalità di lettura
    fp = fopen("test.txt", "r");
    // Controllo se il file è stato aperto correttamente
    if (fp == NULL) {
        printf("Impossibile aprire il file.");
        return 1;
    }
    // Lettura del file carattere per carattere fino alla fine del file
    char ch;
    while ((ch = fgetc(fp)) != EOF) {
        printf("%c", ch);
    }
    // Chiusura del file
    fclose(fp);
    return 0;
}
```

In questo esempio, utilizziamo la funzione `fgetc` per leggere il file carattere per carattere fino alla fine del file, rappresentata dal carattere `EOF` (end of file). È importante ricordare di chiudere il file dopo averlo utilizzato, utilizzando la funzione `fclose`.

## Deep Dive

Per leggere un file di testo in modo più efficiente, è possibile utilizzare la funzione `fread`, che legge un blocco di dati dal file invece di un singolo carattere. Questo può essere utile quando si devono leggere grandi quantità di dati da un file, in modo da ridurre il numero di letture dal disco.

Oltre alla modalità di lettura, `fopen` offre altre opzioni per aprire un file, come la modalità di scrittura o di append. Inoltre, è possibile utilizzare la funzione `fgets` per leggere una riga intera dal file invece di un singolo carattere.

## Vedi anche
- Tutorial su file di testo in C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- Documentazione ufficiale su funzioni di lettura file in C: https://en.cppreference.com/w/c/io/fread