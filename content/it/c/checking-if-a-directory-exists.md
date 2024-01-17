---
title:                "Verificare se una directory esiste"
html_title:           "C: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Controllare se una directory esiste è una pratica comune nella programmazione. Con questo tipo di controllo, si può verificare se una determinata directory esiste o meno, e quindi eseguire operazioni specifiche in base al risultato. I programmatori spesso fanno questo tipo di controllo per evitare errori o conflitti durante l'esecuzione del codice.

## Come fare:

Ecco un esempio di codice in C per controllare se una directory esiste:

```
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int main()
{
    // definisce il nome della directory da controllare
    char* dir_name = "Documents";

    // usa la funzione stat() per ottenere informazioni sulla directory
    struct stat dir_stat;
    if (stat(dir_name, &dir_stat) == 0) {
        // controlla il flag per determinare se la directory esiste
        if (S_ISDIR(dir_stat.st_mode)) {
            printf("La directory '%s' esiste.\n", dir_name);
        } else {
            printf("Non c'è una directory chiamata '%s'.\n", dir_name);
        }
    } else {
        // l'errore potrebbe essere causato da una directory non esistente o problemi di permessi
        printf("Si è verificato un errore durante il controllo della directory '%s'.\n", dir_name);
    }

    return 0;
}
```

Ecco un esempio di output:

```
La directory 'Documents' esiste.
```

## Approfondimento:

### Contesto storico

Il controllo delle directory esiste da molti anni ed è stato introdotto per la prima volta nei sistemi operativi UNIX. Inizialmente, il controllo veniva fatto utilizzando la funzione `access()`, ma in seguito la funzione `stat()` è diventata più comune perché offre maggiori informazioni sulla directory.

### Alternative

Oltre all'utilizzo della funzione `stat()`, ci sono altre alternative per controllare se una directory esiste in C. Ad esempio, si può utilizzare la funzione `opendir()` per aprire una directory e verificare se è stata aperta correttamente. Inoltre, ci sono anche librerie di terze parti che offrono funzionalità per il controllo delle directory.

### Dettagli di implementazione

Per controllare se una directory esiste utilizzando la funzione `stat()`, è necessario passare il nome della directory come argomento della funzione. Il risultato della funzione sarà poi immagazzinato nella struttura `stat`, da cui è possibile estrarre diverse informazioni sulla directory. Utilizzando il flag `st_mode`, è possibile determinare se la directory esiste effettivamente.

## Vedi anche:

- [Funzione stat() in C](https://www.tutorialspoint.com/c_standard_library/c_function_stat.htm)
- [Funzione opendir() in C](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Libreria di terze parti Boost.Filesystem per il controllo delle directory](https://www.boost.org/doc/libs/1_67_0/libs/filesystem/doc/index.htm)