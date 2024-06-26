---
date: 2024-01-20 17:53:52.444741-07:00
description: "How to: (Come fare:) Leggere file di testo \xE8 fondamentale sin dagli\
  \ albori della programmazione. In C++, `ifstream` \xE8 parte della libreria standard\
  \ e sta\u2026"
lastmod: '2024-04-05T22:50:57.540631-06:00'
model: gpt-4-1106-preview
summary: "(Come fare:) Leggere file di testo \xE8 fondamentale sin dagli albori della\
  \ programmazione."
title: Lettura di un file di testo
weight: 22
---

## How to: (Come fare:)
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream infile("esempio.txt");
    std::string line;

    if (infile.is_open()) {
        while (getline(infile, line)) {
            std::cout << line << '\n';
        }
        infile.close();
    } else {
        std::cout << "Impossibile aprire il file." << std::endl;
    }
    return 0;
}
```
Output:
```
Prima riga del file.
Seconda riga del file.
...
```

## Deep Dive (Approfondimento)
Leggere file di testo è fondamentale sin dagli albori della programmazione. In C++, `ifstream` è parte della libreria standard e sta per input file stream. `getline` è preferita per leggere righe perché gestisce automaticamente il buffer.

Alternative a `ifstream` includono la C API (`fopen`, `fgets`, ecc.), ma sono meno sicure e più verbose. `boost::iostreams` e `Qt’s QFile` offrono funzionalità extra per casi d'uso avanzati. Dettagli come la gestione di file binari o encoding diversi possono richiedere attenzioni specifiche.

## See Also (Vedi Anche)
- Documentazione `ifstream`: https://en.cppreference.com/w/cpp/io/basic_ifstream
- Tutorial su file I/O: https://www.cplusplus.com/doc/tutorial/files/
- Boost Iostreams Library: http://www.boost.org/doc/libs/release/libs/iostreams/
