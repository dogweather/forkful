---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa registrare dati sul disco in modo leggibile. I programmatori lo fanno per salvare configurazioni, risultati di esecuzione o per condividere dati fra software.

## How to:
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::string filename = "esempio.txt";
    std::string text = "Ciao, questo è un testo di esempio.";

    std::ofstream myfile(filename);
    if (myfile.is_open()) {
        myfile << text;
        myfile.close();
        std::cout << "File scritto con successo.";
    } else {
        std::cout << "Impossibile aprire il file.";
    }

    return 0;
}
```
*Output:*
`File scritto con successo.`

## Deep Dive
La scrittura su file in C++ risale agli anni '70 con la libreria standard C. Alternativamente, si possono usare librerie C++ moderne come Boost.Iostreams per funzionalità avanzate. Implementare la scrittura su file richiede attenzione alla gestione delle risorse e all'encoding dei dati.

## See Also
- Tutorial C++ sulla gestione dei file: [cplusplus.com](http://www.cplusplus.com/doc/tutorial/files/)
- Documentazione ufficiale di std::ofstream: [cppreference.com](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- Introduzione a Boost.Iostreams: [boost.org](https://www.boost.org/doc/libs/release/libs/iostreams/doc/index.html)