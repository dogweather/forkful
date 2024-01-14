---
title:    "C: Scrivere un file di testo"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è una delle attività fondamentali nella programmazione in C. Un file di testo è un documento che contiene una serie di caratteri e può essere letto o modificato da un programma. Ciò è particolarmente utile per la memorizzazione di grandi quantità di dati o per la comunicazione tra diverse applicazioni.

## Come fare

Per scrivere un file di testo in C, abbiamo bisogno di due cose: un puntatore a un file e una stringa contenente i dati da scrivere. Utilizzando la funzione `fopen` possiamo aprire il file specificando la modalità di scrittura "w", che indica che il file sarà creato se non esiste e sovrascritto se è già presente. A questo punto possiamo utilizzare la funzione `fprintf` per scrivere la stringa nel file aperto. Infine, con la funzione `fclose` chiudiamo il file per salvare le modifiche.

```C
#include <stdio.h>

int main() {
  FILE *file = fopen("file.txt", "w");
  char *string = "Questo è un esempio di stringa da scrivere nel file.";
  fprintf(file, "%s", string);
  fclose(file);
  return 0;
}
```

## Approfondimento

Una volta capito il concetto base della scrittura di un file di testo in C, possiamo approfondire il processo e le altre opzioni disponibili. Ad esempio, possiamo aprire un file in modalità "a", che indica di scrivere alla fine del file anziché sovrascrivere, oppure in modalità "r+", che ci permette di leggere e scrivere nel file contemporaneamente.

Inoltre, è importante gestire gli errori durante il processo di scrittura, ad esempio controllando se il file è stato aperto correttamente prima di iniziare a scrivere o se la scrittura è andata a buon fine. Ciò ci permette di scrivere un codice più robusto e affidabile.

## Vedi anche

- [Documentazione ufficiale di fopen](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Tutorial su come scrivere e leggere da file in C](https://www.programiz.com/c-programming/c-file-input-output)
- [Esempi di codice per la scrittura di file in C](https://www.geeksforgeeks.org/file-handling-c-classes/)