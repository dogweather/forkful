---
title:                "Lavorare con CSV"
aliases:
- /it/c/working-with-csv.md
date:                  2024-02-03T18:11:38.338117-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Nel mondo della programmazione, lavorare con file CSV (Valori Separati da Virgola) implica la lettura e la scrittura di dati in file di testo organizzati per righe, dove ogni riga rappresenta un record e i campi di ogni record sono separati da virgole. I programmatori manipolano i file CSV per facilitare l'importazione/esportazione di dati attraverso vari sistemi, grazie al loro ampio supporto e semplicità per memorizzare dati tabellari.

## Come fare:

### Leggere file CSV
Per leggere un file CSV in C, utilizziamo le funzioni standard di I/O su file insieme alle funzioni di manipolazione delle stringhe per analizzare ogni riga. Di seguito è riportato un esempio di base su come leggere un file CSV e stampare i campi di ogni riga sulla console.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Impossibile aprire il file\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *campo = strtok(buf, ",");
        while(campo) {
            printf("%s\n", campo);
            campo = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Esempio di `data.csv`:
```
Nome,Età,Occupazione
John Doe,29,Ingegnere del Software
```

Esempio di Output:
```
Nome
Età
Occupazione
John Doe
29
Ingegnere del Software
```

### Scrivere su file CSV
Similmente, scrivere su un file CSV implica l'uso di `fprintf` per salvare i dati in formato separato da virgole.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Impossibile aprire il file\n");
        return 1;
    }

    char *intestazioni[] = {"Nome", "Età", "Occupazione", NULL};
    for (int i = 0; intestazioni[i] != NULL; i++) {
        fprintf(fp, "%s%s", intestazioni[i], (intestazioni[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Scienziato dei Dati");

    fclose(fp);
    return 0;
}
```

Contenuto di `output.csv` di esempio:
```
Nome,Età,Occupazione
Jane Doe,27,Scienziato dei Dati
```

## Approfondimento

Il formato CSV, sebbene apparentemente semplice, presenta le sue sfumature, come la gestione delle virgole all'interno dei campi e l'incapsulamento dei campi con le virgolette. Gli esempi rudimentali mostrati non tengono conto di tali complessità, né gestiscono gli errori potenziali in modo robusto.

Storicamente, la gestione dei file CSV in C è stata in gran parte manuale a causa della natura di basso livello del linguaggio e della mancanza di astrazioni di alto livello incorporate per tali compiti. Questa gestione manuale include l'apertura dei file, la lettura delle righe, la divisione delle stringhe e la conversione dei tipi di dati secondo necessità.

Sebbene la manipolazione diretta dei file CSV in C offra preziose esperienze di apprendimento sull'I/O dei file e sulla gestione delle stringhe, diverse alternative moderne promettono efficienza e processi meno inclini agli errori. Librerie come `libcsv` e `csv-parser` offrono funzioni complete per la lettura e la scrittura di file CSV, inclusi il supporto per i campi quotati e i delimitatori personalizzati.

In alternativa, quando si lavora all'interno di ecosistemi che lo supportano, integrarsi con linguaggi o piattaforme che forniscono funzioni di manipolazione CSV di alto livello (come Python con la sua libreria `pandas`) può essere una strada più produttiva per applicazioni che richiedono un pesante elaborazione dei CSV. Questo approccio cross-linguaggio sfrutta le capacità di C in termini di performance e programmazione di sistema, utilizzando allo stesso tempo la facilità di uso di altri linguaggi per compiti specifici come la gestione dei CSV.
