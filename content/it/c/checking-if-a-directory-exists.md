---
title:    "C: Verifica dell'esistenza di una directory"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Se stai lavorando su un programma in linguaggio C che richiede di accedere a una directory, potresti dover verificare prima se questa esiste o meno. Questo passaggio è importante perché può evitare errori nel tuo codice e garantire che il tuo programma funzioni correttamente.

## Come Fare

Per verificare se una directory esiste, è possibile utilizzare la funzione `opendir()` che restituisce un puntatore a una struttura DIR se la directory esiste e `NULL` se non esiste. Di seguito è riportato un esempio di codice che verifica se una directory chiamata "docs" esiste nella directory corrente:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Verifica se la directory "docs" esiste
    if (opendir("docs") != NULL) {
        printf("La directory esiste!\n");
    } else {
        printf("La directory non esiste :(\n");
    }
    return 0;
}
```

L'output di questo programma dipenderà dalla presenza o meno della directory "docs" nella directory corrente:

```
La directory non esiste :(
```

L'uso di `opendir()` potrebbe non essere sufficiente in tutte le situazioni poiché potrebbe essere necessario verificare se la directory è leggibile o scrivibile. In questi casi, è possibile utilizzare la funzione `access()` che consente di verificare un determinato permesso su un file o una directory. Ad esempio, il seguente codice verifica se la directory "docs" esiste e se è leggibile:

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    // Verifica se la directory "docs" esiste e può essere letta
    if (access("docs", R_OK) == 0) {
        printf("La directory esiste ed è leggibile!\n");
    } else {
        printf("La directory non esiste o non è leggibile :(\n");
    }
    return 0;
}
```

## Approfondimento

Per una maggiore precisione nella verifica delle directory, esiste la funzione `stat()` che restituisce una struttura `stat` contenente informazioni dettagliate sul file o sulla directory specificata. Può essere utile quando si deve verificare anche l'esistenza di file all'interno di una determinata directory. Inoltre, è possibile utilizzare le funzioni `mkdir()` e `mkdirp()` per creare una nuova directory nel caso in cui non esista.

## Vedi Anche

- Guida dettagliata alle funzioni di gestione delle directory: [https://www.computerhope.com/unix/uumask.htm](https://www.computerhope.com/unix/uumask.htm)
- Ulteriori esempi di codice per gestire le directory in C: [https://www.tutorialspoint.com/cprogramming/c\_programming\_file\_io.htm](https://www.tutorialspoint.com/cprogramming/c_programming_file_io.htm)
- Documentazione ufficiale sulle funzioni per la gestione delle directory in C: [https://www.gnu.org/software/libc/manual/html\_node/Opening-and-Closing-Directories.html](https://www.gnu.org/software/libc/manual/html_node/Opening-and-Closing-Directories.html)