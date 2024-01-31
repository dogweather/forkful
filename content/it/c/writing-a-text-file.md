---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere un file di testo in C significa registrare dati in un file leggibile. I programmatori lo fanno per salvare configurazioni, dati persistenti o per creare log.

## Come fare:

Ecco un esempio base:

```c
#include <stdio.h>

int main() {
    FILE *file = fopen("output.txt", "w");
    if (file == NULL) {
        return 1;
    }
    fprintf(file, "Ciao, mondo!");
    fclose(file);
    return 0;
}
```

Output in `output.txt`:

```
Ciao, mondo!
```

## Approfondimento

Scrivere su file è un'azione fondamentale in C, fin dai tempi del K&R C, la prima versione del linguaggio. Alternative includono `fwrite` per dati binari e librerie di terze parti. I file aperti con "w" vengono sovrascritti; usa "a" per appendere.

## Vedi Anche

- Documentazione ANSI C: http://www.open-std.org/JTC1/SC22/WG14/
- Tutorial su file I/O in C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- Stack Overflow per dubbi specifici: https://stackoverflow.com/questions/tagged/c?tab=Votes
