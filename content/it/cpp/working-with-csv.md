---
title:                "Lavorare con i file csv."
html_title:           "C++: Lavorare con i file csv."
simple_title:         "Lavorare con i file csv."
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con i file CSV è una delle attività comuni per i programmatori. In breve, il CSV (Comma Separated Values) è un formato di file utilizzato per memorizzare dati in una tabella, dove i dati sono separati da virgole. Ciò rende i CSV facili da leggere e da elaborare per i computer, ed è per questo che i programmatori li utilizzano per gestire grandi quantità di dati.

## Come:
Ecco un esempio semplice di come si può leggere un file CSV utilizzando il C++:

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main(){

    string data;
    
    // apre il file
    ifstream inputFile("dati.csv");

    // verifica se il file è stato aperto correttamente
    if (inputFile.is_open()){

        // legge il file riga per riga
        while (getline(inputFile,data)){

            // stampa i dati contenuti nella riga
            cout << data << endl;
        }
    }

    // chiude il file
    inputFile.close();

    return 0;
}
```

Ecco un esempio di output che si otterrebbe leggendo un file CSV contenente i seguenti dati:

```CSV
Nome, Cognome, Età
Mario, Rossi, 35
Giulia, Bianchi, 28
```

```
Nome, Cognome, Età
Mario, Rossi, 35
Giulia, Bianchi, 28
```

## Approfondimento:
Il formato CSV è stato sviluppato negli anni '70 per gestire grandi quantità di dati in modo semplice e compatto. Inoltre, grazie alla sua struttura semplice, il CSV è facilmente esportabile in altri software come fogli elettronici o database.

Oltre al formato CSV, ci sono anche altri formati di file utilizzati per memorizzare dati in una tabella, come ad esempio il TSV (Tab Separated Values) o il PSV (Pipe Separated Values).

Per implementare la lettura e scrittura di file CSV in un programma C++, si possono utilizzare diverse librerie disponibili online, come ad esempio Libcsv o TinyCSV.

## Vedi anche:
- [TinyCSV](https://github.com/ben-strasser/fast-cpp-csv-parser)