---
title:                "C: Scrittura di un file di testo"
simple_title:         "Scrittura di un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione fondamentale in diversi progetti di programmazione. Spesso si utilizza questa funzione per salvare dati in memoria esterna o per creare un file di configurazione per un programma. In questo articolo vedremo come creare un file di testo utilizzando il linguaggio di programmazione C.

## Come Fare

Per iniziare, importiamo la libreria standard `stdio.h` per poter utilizzare le funzioni di input/output:

```C
#include <stdio.h>
```

Successivamente, definiamo una variabile di tipo puntatore a `FILE` che utilizzeremo per accedere al file che andremo a creare:

```C
FILE *file;
```

Utilizziamo la funzione `fopen()` per aprire un file specificando il nome e la modalità in cui lo vogliamo aprire. In questo caso, creeremo un file di testo in modalità scrittura "w":

```C
file = fopen("test.txt", "w");
```

Ora che il nostro file è aperto, possiamo scrivere al suo interno utilizzando la funzione `fprintf()`, che accetta come argomenti il puntatore al file e ciò che vogliamo scrivere:

```C
fprintf(file, "Questo è un esempio di file di testo scritto in C.");
```

Infine, per assicurarci che tutto sia stato scritto correttamente, dobbiamo chiudere il file utilizzando la funzione `fclose()`:

```C
fclose(file);
```

Ecco un esempio completo di come creare e scrivere un file di testo:

```C
#include <stdio.h>

int main(){
    // Apre il file di testo in modalità scrittura
    FILE *file = fopen("test.txt", "w");

    // Scrive una stringa all'interno del file
    fprintf(file, "Questo è un esempio di file di testo scritto in C.");

    // Chiude il file
    fclose(file);

    return 0;
}
```

## Approfondimento

Oltre alla modalità "w", esistono altre modalità che possono essere utilizzate con la funzione `fopen()` per aprire un file di testo:

- "r" - modalità di lettura, apre il file se esiste altrimenti torna un errore
- "a" - modalità di scrittura in append, appende i dati alla fine del file
- "r+" - modalità di lettura/scrittura, apre il file se esiste altrimenti torna un errore
- "a+" - modalità di lettura/scrittura in append, appende i dati alla fine del file e permette di leggere i dati già presenti

È importante tenere a mente che se si utilizza una modalità di scrittura, come ad esempio "w" o "a", il contenuto del file verrà sovrascritto ogni volta che si apre il file. Mentre le modalità di lettura e di lettura/scrittura non modificano il file.

## Vedi anche

- [Documentazione della funzione `fopen()` in C](https://devdocs.io/c/io/fopen)
- [Tutorial su come scrivere file in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C File I/O Tutorial su Geeksforgeeks](https://www.geeksforgeeks.org/basics-file-handling-c/)