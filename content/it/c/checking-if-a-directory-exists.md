---
title:    "C: Verifica se una directory esiste."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una directory esiste?

Spesso, durante la scrittura di un programma in C, è necessario accedere a file situati in una determinata directory. Prima di provare ad aprire un file, può essere utile verificare se la directory in cui è contenuto esiste effettivamente per evitare errori durante l'esecuzione del programma.

## Come controllare se una directory esiste

Per controllare se una directory esiste in C, possiamo utilizzare la funzione `opendir()` della libreria `<dirent.h>`. Questa funzione prende come parametro il percorso della directory che vogliamo verificare e restituisce un puntatore al suo descrittore, se la directory esiste, o `NULL` se non esiste.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
  DIR *dir = opendir("./my_directory"); // Sostituire con il percorso desiderato
  if (dir) {
    printf("La directory esiste!\n");
    closedir(dir);
  } else {
    printf("La directory non esiste\n");
  }
  return 0;
}
```

Esempio di output:

```
La directory non esiste
```

## Approfondimento sul controllo delle directory

È importante notare che la funzione `opendir()` verifica solo l'esistenza della directory, ma non garantisce che la directory sia effettivamente accessibile. Inoltre, è possibile utilizzare la funzione `mkdir()` per creare una nuova directory, se non esiste, in modo da evitare eventuali errori durante l'esecuzione del programma.

## Vedi anche

- Guida completa alla libreria <dirent.h>: https://www.tutorialspoint.com/c_standard_library/dirent_h.htm
- Esempi di utilizzo della funzione `opendir()`: https://www.codingame.com/playgrounds/14213/system-calls-in-unix/directory-operations
- Documentazione ufficiale della funzione `opendir()`: https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html