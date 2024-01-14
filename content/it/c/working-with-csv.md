---
title:                "C: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con dati tabulari, è molto probabile che tu abbia avuto a che fare con il formato CSV. I file CSV (Comma-Separated Values) sono un modo molto comune per archiviare e condividere dati strutturati in una forma leggibile sia per esseri umani che per computer. Imparare a lavorare con CSV può essere molto utile per gestire grandi quantità di dati in modo efficiente.

## Come Fare

Per lavorare con file CSV in C, è necessario utilizzare una libreria esterna come "csvparser" o "libcsv". In questo esempio, useremo la libreria "csvparser" per leggere e scrivere dati da e su file CSV.

```C
#include <stdio.h>
#include "csvparser.h"

int main() {
    FILE *csv_file;
    char *csv_line;
    CsvParser *csv_parser;
    CsvRow *csv_row;

    // Apriamo il file CSV in modalità di lettura
    csv_file = fopen("dati.csv", "r");

    // Creiamo un nuovo parser per il file CSV
    csv_parser = CsvParser_new("dati.csv", ",", 1);

    // Finché ci sono righe nel file, leggiamole una alla volta
    while ((csv_row = CsvParser_getRow(csv_parser))) {
        // Stampiamo il numero di campi nella riga
        printf("Numero di campi nella riga: %d\n", CsvParser_getNumFields(csv_row));

        // Leggiamo ed elaboriamo i dati di ogni campo
        const char **csv_fields = CsvParser_getFields(csv_row);

        for (int i = 0 ; i < CsvParser_getNumFields(csv_row) ; i++) {
            // Stampiamo il valore del campo
            printf("Campo %d: %s\n", i, csv_fields[i]);
        }

        // Distruggiamo la riga corrente
        CsvParser_destroy_row(csv_row);
    }

    // Chiudiamo il file e distruggiamo il parser
    fclose(csv_file);
    CsvParser_destroy(csv_parser);

    return 0;
}
```
Output:
```
Numero di campi nella riga: 3
Campo 0: John
Campo 1: Smith
Campo 2: 25
Numero di campi nella riga: 3
Campo 0: Jane
Campo 1: Doe
Campo 2: 30
Numero di campi nella riga: 3
Campo 0: Bob
Campo 1: Johnson
Campo 2: 35
```

## Approfondimento

La libreria "csvparser" offre diverse funzionalità utili per lavorare con file CSV. Ad esempio, è possibile specificare il delimitatore dei campi e se il file ha un'intestazione. Inoltre, la libreria gestisce automaticamente il rilevamento delle righe vuote e dei valori citati tra virgolette. Per ulteriori informazioni, consulta la documentazione della libreria.

La gestione dei file CSV può diventare piuttosto complessa se si considerano i diversi formati di delimitazione dei campi, le possibili righe vuote e le citazioni dei valori. Tuttavia, una volta padroneggiata la libreria "csvparser", lavorare con CSV diventerà più semplice e sicuro.

## Vedi Anche

- [Documentazione della libreria csvparser](https://github.com/robertpostill/csvparser)
- [Esempi di utilizzo della libreria csvparser](https://github.com/robertpostill/csvparser/tree/master/tests)
- [Tutorial sulla gestione dei file CSV in C](https://www.programmingworld.tech/2018/11/working-with-csv-in-c.html)