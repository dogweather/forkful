---
title:                "Scrivere un file di testo"
html_title:           "C++: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere un file di testo in linguaggio C++ significa creare un documento di testo che può essere letto e modificato da un programma. I programmatori utilizzano questa tecnica per salvare dati ottenuti dall'esecuzione del programma, in modo da poterli riutilizzare in futuro.

## Come fare:

```
#include <fstream>
using namespace std;

int main()
{
    // Creiamo un nuovo file di testo
    ofstream file("output.txt");

    // Scriviamo alcuni dati nel file
    file << "Questo è un esempio di file di testo creato con C++. \n";
    file << "I file di testo possono contenere dati di qualsiasi tipo, come numeri, stringhe o caratteri speciali. \n";

    // Chiudiamo il file
    file.close();

    return 0;
}
```

Output: Il file output.txt verrà creato e conterrà le due righe di testo indicate nel codice.

## Approfondimento:

Scrivere un file di testo è un'operazione molto comune nella programmazione. Ciò è dovuto alla sua semplicità e alla sua compatibilità con molti linguaggi di programmazione diversi. Alcune alternative a questa tecnica includono l'utilizzo di database o di altri formati di file, come XML o JSON. Per scrivere un file di testo in C++, è necessario utilizzare il filestream ```ofstream```, che è una classe appartenente alla libreria standard <fstream>.

## Vedi anche:

- [Documento ufficiale di <fstream>](https://en.cppreference.com/w/cpp/header/fstream)
- [Tutorial su come usare <fstream>](https://www.geeksforgeeks.org/file-handling-c-classes/)