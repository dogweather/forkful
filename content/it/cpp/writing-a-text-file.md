---
title:                "Scrittura di un file di testo"
html_title:           "C++: Scrittura di un file di testo"
simple_title:         "Scrittura di un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui uno potrebbe voler scrivere un file di testo in un programma C++. Ad esempio, potresti aver bisogno di salvare dei dati su un file per leggerli in un secondo momento o per condividerli con altri programmi.

## Come fare

Per iniziare a scrivere un file di testo in C++, è necessario includere la libreria `fstream` e creare un oggetto `ofstream`. Poi, è possibile utilizzare il metodo `open()` per aprire un file esistente o crearne uno nuovo da scrivere. Esempio:

```
#include <fstream>
using namespace std;

int main() {
    ofstream file; // creare l'oggetto ofstream
    file.open("file.txt"); // aprire/creare il file "file.txt"
    
    // scrivere del testo nel file
    file << "Questo è un esempio di testo scritto in un file.";
    
    // chiudere il file
    file.close();
    
    return 0;
}
```

Dopo l'esecuzione del codice, verrà creato un file chiamato "file.txt" con il testo all'interno. Se il file esisteva già, il testo precedente verrà sovrascritto.

## Approfondimento

Esistono diverse opzioni per la scrittura di file di testo in C++. Ad esempio, si può specificare il percorso di un file tramite il costruttore dell'oggetto `ofstream` o utilizzare l'operatore `<<` per scrivere variabili o espressioni. Inoltre, è possibile impostare il flag `ios::app` nell'apertura del file per aggiungere il testo alla fine, anziché sovrascrivere quello precedente.

## Vedi anche

- [Tutorial: Lettura e scrittura di file in C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Documentazione ufficiale di C++ per la gestione dei file](https://en.cppreference.com/w/cpp/io/basic_filebuf)