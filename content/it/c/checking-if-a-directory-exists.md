---
title:                "C: Verifica se una cartella esiste"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché 

Quando si scrive un programma in C, ci si trova spesso nella situazione di dover verificare se una determinata directory esiste o meno. Questa informazione può essere importante per garantire il corretto funzionamento del programma o per gestire gli errori in modo adeguato.

## Come Fare

Per verificare se una directory esiste in C, possiamo utilizzare la funzione `opendir()` della libreria `<dirent.h>`. Questa funzione prende come argomento il percorso della directory da controllare e restituisce un puntatore di tipo `DIR`. Se la directory esiste, il puntatore restituito non sarà NULL, altrimenti sarà NULL.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // directory da controllare
    char* dir_path = "/path/to/directory";

    // apri la directory
    DIR* dir = opendir(dir_path);

    // verifica se la directory esiste
    if (dir) {
        printf("%s esiste \n", dir_path);
        closedir(dir);
    }
    else {
        printf("%s non esiste \n", dir_path);
    }

    return 0;
}
```

Output:

```bash
/path/to/directory esiste
```

## Approfondimento

Se vogliamo ottenere più informazioni sulla directory, possiamo utilizzare la funzione `stat()` della libreria `<sys/stat.h>`. Questa funzione prende come argomento il percorso della directory e una struttura `stat` dove verranno salvati i dati relativi alla directory, come la dimensione, la data di creazione e altre informazioni utili.

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    // directory da controllare
    char* dir_path = "/path/to/directory";

    // struttura per i dati della directory
    struct stat dir_info;

    // verifica se la directory esiste e ottieni i dati
    if (stat(dir_path, &dir_info) == 0) {
        printf("Dimensione: %ld bytes \n", dir_info.st_size);
        printf("Data di creazione: %ld \n", dir_info.st_ctime);
    }
    else {
        printf("%s non esiste \n", dir_path);
    }

    return 0;
}
```

Output:

```bash
Dimensione: 4096 bytes
Data di creazione: 1585225447
```

## Vedi Anche

- [Tutorial su opendir() e readdir()](https://www.programiz.com/c-programming/c-file-input-output)
- [Documentazione della funzione stat()](https://www.gnu.org/software/libc/manual/html_node/Status-of-Files.html)
- [Manipolazione di directory in C](https://www.geeksforgeeks.org/c-programming-list-files-sub-directories-directory/)