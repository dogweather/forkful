---
title:                "Verifica se una directory esiste"
html_title:           "C: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
In programmazione C, verificare se una directory esiste è un compito comune eseguito per garantire che i dati possano essere salvati in modo sicuro o recuperati da una posizione specifica. Gli sviluppatori fanno questo per evitare errori di runtime.

## Come fare:
L'operazione può essere eseguita utilizzando la funzione stat() del C. Ecco un esempio:

```C
#include <sys/stat.h>
#include <stdio.h>

int is_dir_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != -1) {
       if (S_ISDIR(statbuf.st_mode)) {
           return 1;
       }
    }
    return 0;
}

int main() {
    const char *path = "./test_dir";
    if (is_dir_exists(path)) {
        printf("Directory exists.\n");
    } else {
        printf("Directory doesn't exist.\n");
    }
    return 0;
}
```
Output campione:

```C
Directory exists.
```

## Approfondimento
La funzione `stat()` utilizzata qui è parte della libreria POSIX C ed è disponibile da molto tempo, rendendola una soluzione affidabile. Tuttavia, ci potrebbero essere alternative a `stat()`, come `opendir()` che è più recente. Tieni presente che `stat()` e `opendir()` potrebbero comportarsi diversamente su differenti sistemi operativi o file system.

Una volta richiamata la funzione `stat()`, si riempie una struttura `stat` con informazioni sul percorso. La macro `S_ISDIR()` controlla se il percorso rappresenta una directory.

## Vedi Anche
- Documentazione di `stat()`: [http://man7.org/linux/man-pages/man2/stat.2.html](http://man7.org/linux/man-pages/man2/stat.2.html)
- Documentazione di `opendir()`: [http://man7.org/linux/man-pages/man3/opendir.3.html](http://man7.org/linux/man-pages/man3/opendir.3.html)
- Dettagli sulla struttura `stat` e la macro `S_ISDIR()`: [https://pubs.opengroup.org/onlinepubs/007904975/basedefs/sys/stat.h.html](https://pubs.opengroup.org/onlinepubs/007904975/basedefs/sys/stat.h.html)