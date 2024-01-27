---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Verificare l'esistenza di una directory significa controllare se una certa cartella è presente nel file system. I programmatori lo fanno per evitare errori come la lettura o la scrittura in una directory inesistente, che potrebbe causare crash del programma.

## How to: (Come fare:)
```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat statbuf;
    const char *dirname = "/percorso/alla/directory";
    
    // Il risultato di stat() sarà 0 se la directory esiste
    if (stat(dirname, &statbuf) == 0 && S_ISDIR(statbuf.st_mode)) {
        printf("La directory esiste.\n");
    } else {
        printf("La directory non esiste.\n");
    }
    
    return 0;
}
```
Output possibile:
```
La directory esiste.
```
o
```
La directory non esiste.
```

## Deep Dive (Approfondimento)
Historically, checking for the existence of directories or files in C has been a common task, with various methods evolving over time. Initially, programmers may have used system calls directly, but as the C Standard Library expanded, higher-level functions like `stat()` became the norm for such tasks.

`stat()` è utile perché restituisce informazioni sulla file, che poi controllo per determinare se è una directory. Alternativamente, le funzioni `opendir()` e `closedir()` da `dirent.h` possono essere usate, ma queste sono specifiche per sistemi UNIX.

I dettagli d'implementazione possono variare da un sistema operativo all'altro, per cui è importante controllare la documentazione specifica del proprio ambiente. 

Note that checking for a directory doesn't guarantee future operations, like writing to it, will succeed. Permissions might not allow writing, or the directory could be deleted or moved between the check and the write operation.

## See Also (Vedi Anche)
- [More about file system operations in C](https://en.wikipedia.org/wiki/C_file_input/output)
