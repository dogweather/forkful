---
title:                "Trasformare una stringa in minuscolo"
html_title:           "C++: Trasformare una stringa in minuscolo"
simple_title:         "Trasformare una stringa in minuscolo"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Convertire una stringa in minuscolo è un'operazione molto comune nella programmazione. Consiste nel trasformare tutte le lettere maiuscole di una stringa in lettere minuscole. I programmatori spesso fanno questo per ragioni di uniformità, per una migliore corrispondenza tra stringhe di input o semplicemente per motivi estetici.

## Come fare:
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string str = "CIAO A TUTTI!";
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    std::cout << str; //output: ciao a tutti!
    return 0;
}
```

## Approfondimenti:
Ci sono diversasoluzioni per convertire una stringa in minuscolo. Una delle prime è stata l'uso della libreria string.h che contiene la funzione strlwr(). Tuttavia, questa funzione potrebbe avere comportamenti imprevedibili su alcune piattaforme. Altre opzioni includono l'uso delle funzioni di libreria di C++ come std::tolower() o std::transform(). È importante notare che quando si lavora con caratteri speciali, come ad esempio lettere accentate, l'implementazione della conversione in minuscolo potrebbe non essere consistente tra diverse lingue e piattaforme.

## Vedi anche:
- [Funzione strlwr() della libreria string.h](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Funzione std::tolower() della libreria di C++](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [Funzione std::transform() della libreria di C++](https://en.cppreference.com/w/cpp/algorithm/transform)