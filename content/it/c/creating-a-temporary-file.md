---
title:                "C: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'operazione fondamentale per i programmatori in C. Questi file vengono utilizzati per archiviare dati temporanei durante l'esecuzione di un programma e poi vengono eliminati, riducendo così il carico sul sistema.

## Come fare

Il processo per creare un file temporaneo in C è relativamente semplice. Innanzitutto, dobbiamo includere la libreria "stdio.h" nel nostro codice per poter utilizzare le funzioni di input/output standard. Inoltre, dovremo importare la libreria "stdlib.h" perché utilizzeremo la funzione "tmpfile()" per creare il file temporaneo.

```C
#include <stdio.h>
#include <stdlib.h>

FILE *temp_file = tmpfile(); //crea il file temporaneo
```

La funzione "tmpfile()" accetta due argomenti: il percorso del file e la modalità di apertura. Se il percorso viene impostato su NULL, il file temporaneo verrà salvato nella directory di sistema predefinita. Inoltre, il flag di modalità di apertura (ad esempio "w", "r", "a") determina se il file verrà creato per la scrittura, la lettura o l'aggiunta di dati.

Possiamo poi scrivere o leggere dati nel file temporaneo utilizzando le funzioni di input/output standard come "fprintf()" o "fscanf()" rispettivamente.

```C
fprintf(temp_file, "Questo è un esempio di scrittura di dati nel file temporaneo.");
```

Infine, dobbiamo chiudere il file temporaneo e rimuoverlo utilizzando la funzione "fclose()".

```C
fclose(temp_file); //chiude il file temporaneo
```

## Approfondimento

Oltre alla funzione "tmpfile()", esistono altre alternative per la creazione di file temporanei in C, come ad esempio la funzione "mkstemp()" che permette di specificare un prefisso per il nome del file. Inoltre, è possibile specificare una directory di lavoro in cui verrà creato il file temporaneo anziché utilizzare la directory di sistema predefinita.

La gestione dei file temporanei è un aspetto importante della programmazione in C, poiché la loro corretta creazione e gestione può aiutare a migliorare le prestazioni del nostro programma.

## Vedi anche

- [Documentazione delle funzioni "tmpfile()" e "fclose()"](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Esempi pratici di creazione e gestione di file temporanei in C](https://www.geeksforgeeks.org/temporary-file-creation-in-c/)
- [Tutorial su come utilizzare la funzione "mkstemp()" per creare file temporanei in C](https://www.thegeekstuff.com/2012/06/temp-file-creation-in-c/)