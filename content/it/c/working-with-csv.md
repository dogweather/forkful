---
title:                "Manipolazione dei file csv"
html_title:           "C: Manipolazione dei file csv"
simple_title:         "Manipolazione dei file csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-csv.md"
---

{{< edit_this_page >}}

Ciao a tutti i lettori!

## Cos'è e perché?

CSV sta per Comma-Separated Values ed è un formato di file comunemente utilizzato per archiviare dati tabulari. In poche parole, è un modo per organizzare i dati in righe e colonne, dove i valori sono separati da una virgola. I programmatori spesso lavorano con file CSV perché sono facili da leggere e scrivere, rendendo il processo di analisi dei dati più veloce e semplice.

## Come fare:

Per lavorare con file CSV in C, abbiamo bisogno di includere la libreria "stdio.h". Possiamo quindi utilizzare la funzione "fopen" per aprire il file CSV e leggere i dati utilizzando la funzione "fscanf". Di seguito un esempio di codice:

```
#include <stdio.h>

int main() {
    FILE *csv_file;
    int id;
    char name[50];
    int age;
    
    // Apriamo il file csv
    csv_file = fopen("dati.csv", "r");

    // Leggiamo i dati utilizzando fscanf
    while (fscanf(csv_file, "%d,%s,%d", &id, name, &age) != EOF) {
        printf("ID: %d\n", id);
        printf("Nome: %s\n", name);
        printf("Età: %d\n", age);
    }

    // Chiudiamo il file
    fclose(csv_file);
    
    return 0;
}
```

Output:

```
ID: 1
Nome: Luca
Età: 25
ID: 2
Nome: Maria
Età: 30
ID: 3
Nome: Marco
Età: 28
```

## Approfondimento:

Il formato CSV è stato introdotto nella metà degli anni '70 e da allora è diventato uno dei formati più popolari per lo scambio di dati tra applicazioni. Esistono anche altri formati di file tabulari come TSV (Tab-Separated Values), ma CSV è rimasto il più diffuso grazie alla sua semplicità e compatibilità con la maggior parte dei programmi.

Inoltre, ci sono diverse librerie disponibili per lavorare con file CSV in C, come ad esempio "libcsv" e "libcsv-parser". È importante notare che quando si lavora con file CSV, bisogna prestare attenzione alle virgole all'interno dei valori, in quanto possono interferire con la corretta lettura dei dati.

## Vedi anche:

- [Funzioni di input/output della libreria stdio.h](https://www.programiz.com/c-programming/c-input-output)
- [Libcsv - Libreria C per lavorare con file CSV](https://sourceforge.net/projects/libcsv/)
- [Libcsv-parser - Libreria C per parsing di file CSV](https://github.com/evanphx/libcsv-parser)