---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con file CSV significa manipolare dati in un formato testuale semplice, usato comunemente per l'esportazione e l'importazione tra fogli di calcolo e database. Programmatori lo fanno per la sua semplicità e universalità - quasi ogni piattaforma può leggerlo e scriverlo.

## How to:
Ecco un esempio di come leggere un file CSV in C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024

int main() {
    FILE *file = fopen("esempio.csv", "r");
    if (!file) {
        perror("Impossibile aprire il file");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, MAX_LINE_LENGTH, file)) {
        // Assumendo che i campi siano separati da virgole
        char *token = strtok(line, ",");
        while (token) {
            printf("%s\n", token);
            token = strtok(NULL, ",");
        }
    }

    fclose(file);
    return EXIT_SUCCESS;
}
```

Output (assumendo che `esempio.csv` abbia due righe di dati):
```
primo_campo_prima_riga
secondo_campo_prima_riga
primo_campo_seconda_riga
secondo_campo_seconda_riga
```

## Deep Dive
CSV sta per "Comma-Separated Values" (valori separati da virgola). Emerge nei primi anni di informatica ed è uno standard de facto per scambio dati. Ci sono alternative come XML e JSON, più strutturati, ma CSV rimane popolare per la sua leggibilità e semplicità. Importante è gestire le virgole nei dati, le righe vuote e i campi con ritorni a capo.

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180), che definisce il formato CSV standard.
- [libcsv](http://sourceforge.net/projects/libcsv/), una libreria C per il parsing di CSV.
- [Tutorial su file I/O in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm), per approfondimenti su letture e scritture di file.
