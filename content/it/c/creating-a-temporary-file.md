---
title:                "Creazione di un file temporaneo"
html_title:           "C: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Creare un file temporaneo è una tecnica comune utilizzata dai programmatori per gestire dati temporanei o per creare una copia di un file esistente. Questo permette di manipolare i dati senza modificare il file originale.

## Come fare:

Per creare un file temporaneo in C, è necessario includere la libreria "stdio.h" e utilizzare la funzione "tmpfile()". Qui di seguito un esempio di codice:

```C
#include <stdio.h>

int main()
{
    FILE *file_ptr;
    file_ptr = tmpfile();
    
    if (file_ptr == NULL)
    {
        printf("Errore nella creazione del file temporaneo");
        return 1;
    }
    
    fprintf(file_ptr, "Questo è un esempio di file temporaneo");
    
    fclose(file_ptr);
    
    return 0;
}
```

L'output di questo esempio sarà un file chiamato "tmp[caratteri casuali]" che contiene il testo specificato.

## Approfondimento:

La creazione di file temporanei è una pratica comune nella programmazione, specialmente quando si lavora con dati sensibili o si vogliono effettuare modifiche senza alterare il file originale. Tuttavia, questa tecnica può anche essere utilizzata per scopi malevoli, come ad esempio creare virus o malware. In alternativa alla funzione "tmpfile()", esistono anche altre modalità per creare file temporanei, come ad esempio utilizzare una directory di sistema predefinita o generare nomi di file casuali.

## Vedi anche:

Per ulteriori informazioni sulla creazione di file temporanei in C, puoi consultare la documentazione ufficiale su [tmpfile()](https://en.cppreference.com/w/c/io/tmpfile) e [file handling in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm). Inoltre, puoi esplorare altre alternative alla funzione "tmpfile()" come [tmpnam()](https://en.cppreference.com/w/c/io/tmpnam) o [mkstemp()](https://www.tutorialspoint.com/c_standard_library/c_function_mkstemp.htm).