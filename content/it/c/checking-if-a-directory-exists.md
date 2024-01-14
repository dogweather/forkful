---
title:                "C: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando programmi in C, potresti aver bisogno di controllare se una directory esiste prima di eseguire determinate operazioni su di essa. Ciò può essere utile per garantire che il programma funzioni correttamente e non si verifichino errori imprevisti.

## Come

Per prima cosa, è necessario includere la libreria `dirent.h` nel tuo programma. Questa libreria contiene le funzioni necessarie per verificare l'esistenza di una directory.

Dopo aver incluso la libreria, è possibile utilizzare la funzione `opendir()` per aprire una directory esistente. Se la funzione restituisce un puntatore NULL, significa che la directory non esiste. In caso contrario, significa che la directory esiste ed è stata aperta correttamente.

Di seguito è riportato un esempio di codice che utilizza la funzione `opendir()` per verificare l'esistenza di una directory:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("esempio/");

    if (dir == NULL) {
        printf("La directory non esiste\n");
    } else {
        printf("La directory esiste\n");
        closedir(dir);
    }

    return 0;
}
```

Se esegui questo programma, dovresti ottenere l'output "La directory esiste", poiché nella directory "esempio" esiste una cartella.

## Approfondimenti

La funzione `opendir()` è solo una delle opzioni disponibili per verificare l'esistenza di una directory. Altre funzioni utili sono `access()`, che controlla i permessi di accesso alla directory, e `stat()`, che restituisce informazioni sul file system.

Inoltre, ci sono alcune considerazioni da tenere in mente quando si lavora con directories. Ad esempio, assicurati che il percorso fornito alla tua funzione sia corretto e che non includa spazi o caratteri speciali. Inoltre, gestisci gli errori e le eccezioni in modo appropriato per evitare potenziali crash del programma.

## Vedi Anche

- [Documentazione dirent.h](https://www.gnu.org/software/libc/manual/html_node/Directories.html)
- [Esempi di codice per verificare l'esistenza di una directory in C](https://www.thegeekstuff.com/2014/03/c-check-if-file-exists/)