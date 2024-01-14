---
title:                "C: Creazione di un file temporaneo"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe voler creare un file temporaneo nel corso del suo lavoro. Alcune delle motivazioni più comuni includono la necessità di archiviare dati temporanei, gestire la memoria di un programma o lavorare con file di grandi dimensioni.

## Come fare

Per creare un file temporaneo in C, è necessario utilizzare la funzione `tmpfile()` che è definita nella libreria standard `stdio.h`. Ecco un esempio di come utilizzarla:

```C
#include <stdio.h>

int main()
{
    FILE *temp_file;
    temp_file = tmpfile(); // crea un file temporaneo
    if (temp_file != NULL)
    {
        fprintf(temp_file, "Questo è un esempio di scrittura su file temporaneo!");
        printf("File temporaneo creato con successo!\n");
    }
    else
    {
        printf("Errore nella creazione del file temporaneo.\n");
    }
    return 0;
}
```

L'output di questo programma dovrebbe essere:

```
File temporaneo creato con successo!
```

## Approfondimento

La funzione `tmpfile()` crea un file temporaneo all'interno della directory temporanea del sistema. Questo significa che il file potrebbe essere rimosso automaticamente dal sistema in qualsiasi momento. Tuttavia, è possibile utilizzare la funzione `ftmpfile()` per creare un file temporaneo in una directory specifica.

Inoltre, è possibile utilizzare la funzione `getchar()` per leggere i dati da un file temporaneo e la funzione `fseek()` per posizionarsi in un punto specifico all'interno del file. È importante ricordare di chiudere il file temporaneo utilizzando la funzione `fclose()` una volta terminato di utilizzarlo.

## Vedi anche

- [Funzione tmpfile()](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Come gestire i file in C](https://www.programiz.com/c-programming/c-file-input-output)
- [Guida alla libreria standard di C](https://www.tutorialspoint.com/c_standard_library/index.htm)