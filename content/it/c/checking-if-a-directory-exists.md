---
title:                "Verifica se una cartella esiste"
html_title:           "C: Verifica se una cartella esiste"
simple_title:         "Verifica se una cartella esiste"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario verificare se una directory esiste prima di eseguire un'operazione. Ad esempio, quando si desidera creare una nuova directory, copiare o spostare un file in una directory esistente o semplicemente assicurarsi che il percorso specificato sia valido.

## Come fare

Per verificare se una directory esiste in C, possiamo utilizzare la funzione `opendir()` della libreria `dirent.h`. Questa funzione prende in input il percorso della directory e restituisce un puntatore di tipo `DIR`. Se il puntatore restituito è `NULL`, significa che la directory non esiste.

```
#include <stdio.h>
#include <dirent.h>

int main() {
    // Specificare il percorso della directory da verificare
    char* path = "/home/utente/Documents";
    
    // Aprire la directory con opendir()
    DIR* dir = opendir(path);
    
    // Verificare se il puntatore restituito è NULL
    if (dir == NULL) {
        printf("La directory non esiste\n");
    }
    else {
        printf("La directory esiste\n");
        
        // Chiudere la directory con closedir()
        closedir(dir);
    }
    
    return 0;
}
```

Output:

```
La directory esiste
```

Per ulteriori esempi e dettagli sull'utilizzo della funzione `opendir()`, si consiglia di consultare la documentazione ufficiale.

## Approfondimento

Oltre alla funzione `opendir()`, esistono altre varianti per verificare se una directory esiste in C, come ad esempio la funzione `stat()` della libreria `sys/stat.h`. Questa funzione restituisce informazioni sul file/directory specificato, compresa una struttura `stat` che contiene il campo `st_mode` contenente informazioni sul tipo di file. Se il campo `st_mode` contiene il flag `S_IFDIR`, significa che si tratta di una directory.

Una nota importante da tenere a mente è che queste funzioni possono fornire risultati diversi in base ai permessi del file system. Ad esempio, se una directory è accessibile in sola lettura, il programma potrebbe considerarla non esistente, anche se fisicamente è presente sul disco. In questi casi è importante gestire gli errori adeguatamente e gestire i permessi correttamente per garantire il corretto funzionamento del programma.

## Vedi anche

- Documentazione ufficiale della funzione `opendir()`: https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html
- Documentazione ufficiale della funzione `stat()`: https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html#File-Attributes
- Esempi di gestione degli errori e dei permessi nell'utilizzo di queste funzioni: https://www.thegeekstuff.com/2012/06/c-directory/