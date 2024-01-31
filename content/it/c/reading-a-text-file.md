---
title:                "Lettura di un file di testo"
date:                  2024-01-20T17:53:48.348474-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura di un file di testo"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Leggere un file di testo in C significa poter accedere e lavorare con il contenuto salvato su disco. I programmatori lo fanno per manipolare dati, configurare software, o per importare dati esterni nel programma.

## How to: (Come fare:)
```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char line[255]; // buffer per la linea
    
    file = fopen("esempio.txt", "r"); // apri il file in modalità lettura
    
    if (file == NULL) {
        printf("Errore nell'apertura del file.\n");
        return 1;
    }
    
    while (fgets(line, sizeof(line), file)) {
        printf("%s", line); // stampa ogni linea
    }
    
    fclose(file); // chiudi il file
    return 0;
}
```
**Output dell'esempio:**
```
Prima riga del file.
Seconda riga del file.
Terza riga del file.
```

## Deep Dive (Approfondimento)
La funzione `fopen()` è stata introdotta nello standard C con lo scopo di aprire file per la lettura/scrittura. `fgets()` legge righe di testo fino al raggiungimento di un newline o EOF. Alternative includono `fread()` per dati binari, `getline()` in GNU C, e `fscanf()` per formati specifici. La gestione degli errori è cruciale: controlla sempre il valore di ritorno di `fopen()` e, in produzione, gestisci i permessi dei file e altri potenziali problemi di sicurezza.

## See Also (Vedi Anche)
- [cplusplus.com - Input/output with files](http://www.cplusplus.com/doc/tutorial/files/)
- [Stack Overflow - Reading a file line by line](https://stackoverflow.com/questions/3501338/c-read-file-line-by-line)
