---
title:                "Creazione di un file temporaneo"
html_title:           "C++: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si scrive un programma in C++, può essere utile creare un file temporaneo. Questo può essere fatto per una varietà di motivi, come memorizzare dati temporanei, evitare conflitti di nomi dei file o semplicemente per una maggiore efficienza nell'esecuzione del codice.

## Come fare

Per creare un file temporaneo in C++, è necessario includere la libreria <cstdio> e utilizzare la funzione "tmpfile()" come mostrato di seguito:

```C++
#include <cstdio>      // Includiamo la libreria
...
FILE *f = tmpfile();   // Creiamo il file temporaneo utilizzando la funzione "tmpfile()"
```

È importante notare che la funzione "tmpfile()" restituisce un puntatore al file creato, quindi è necessario assegnarlo a una variabile di tipo FILE*. Una volta che il file è stato creato, è possibile utilizzare le funzioni standard della libreria <cstdio> per scrivere e leggere dati dal file.

```C++
fprintf(f, "Questo è un esempio di scrittura su un file temporaneo.\n");   // Scriviamo una stringa sul file
fseek(f, 0, SEEK_SET);   // Spostiamoci all'inizio del file
char str[100];   // Definiamo una stringa di dimensione sufficiente per contenere i dati letti dal file
fgets(str, 100, f);   // Leggiamo i dati dal file e li memorizziamo nella stringa
printf("Dati letti dal file: %s", str);   // Stampiamo a schermo i dati letti
```

L'output di questo esempio sarà:

```
Dati letti dal file: Questo è un esempio di scrittura su un file temporaneo.
```

Una volta che il programma è terminato, è importante ricordare di chiudere il file temporaneo utilizzando la funzione "fclose()".

## Approfondimento

Creare un file temporaneo utilizzando la funzione "tmpfile()" è solo una delle opzioni disponibili in C++. Esistono anche altre funzioni come "tmpnam()" che restituiscono una stringa contenente un nome univoco per il file temporaneo. Inoltre, è possibile specificare un percorso di destinazione per il file temporaneo utilizzando la funzione "tmpfile_s()" che richiede anche la specifica della dimensione massima del percorso e del nome del file.

## Vedi anche

- Articolo sulla gestione dei file in C++: https://www.cplusplus.com/doc/tutorial/files/
- Tutorial su come utilizzare la libreria <cstdio> in C++: https://www.codecademy.com/learn/learn-c-plus-plus/modules/introduction-to-cpp/cheatsheet