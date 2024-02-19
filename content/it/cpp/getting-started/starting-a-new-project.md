---
aliases:
- /it/cpp/starting-a-new-project/
date: 2024-01-20 18:03:10.369678-07:00
description: "Iniziamo un nuovo progetto per trasformare le idee in codice funzionante.\
  \ I programmatori lo fanno per risolvere problemi, esplorare nuove tecnologie o\u2026"
lastmod: 2024-02-18 23:08:56.173049
model: gpt-4-1106-preview
summary: "Iniziamo un nuovo progetto per trasformare le idee in codice funzionante.\
  \ I programmatori lo fanno per risolvere problemi, esplorare nuove tecnologie o\u2026"
title: Avvio di un nuovo progetto
---

{{< edit_this_page >}}

## Cosa & Perché?
Iniziamo un nuovo progetto per trasformare le idee in codice funzionante. I programmatori lo fanno per risolvere problemi, esplorare nuove tecnologie o semplicemente per imparare.

## Come fare:
Creare un nuovo progetto in C++ con CMake, uno strumento di build standard. Immaginiamo di voler creare un'app "Hello, World!".

```C++
// File: main.cpp
#include <iostream>

int main() {
    std::cout << "Ciao, Mondo!" << std::endl;
    return 0;
}
```

Ecco il CMakeLists.txt minimo necessario:

```C++
// CMakeLists.txt
cmake_minimum_required(VERSION 3.10)
project(MioProgetto)

set(CMAKE_CXX_STANDARD 17)
add_executable(MioProgetto main.cpp)
```

Compilare e eseguire:

```bash
mkdir build && cd build
cmake ..
make
./MioProgetto
```

Output:

```
Ciao, Mondo!
```

## Approfondimento
Il C++ continua a evolversi. L'uso di CMake è diventato popolare perché standardizza il processo di build su diverse piattaforme. Prima di CMake, gli sviluppatori scrivevano makefile complessi o utilizzavano strumenti specifici per il sistema operativo. CMake astrae queste complessità.

Conformarsi allo standard C++ più recente (ad esempio, C++20) assicura l'uso delle ultime funzionalità come i concept per la programmazione generica più semplice e i moduli per migliorare i tempi di compilazione.

È anche possibile usare altri strumenti di build come Meson o Bazel, o creare un progetto all'interno di un IDE che gestisca la configurazione per te, come Visual Studio o CLion. Tuttavia, imparare CMake è utile per la sua portabilità e ampio uso.

## Vedi Anche
- [CMake Documentation](https://cmake.org/documentation/)
- [Modern CMake](https://cliutils.gitlab.io/modern-cmake/)
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [cppreference.com](https://en.cppreference.com/w/) per la documentazione standard del C++.
