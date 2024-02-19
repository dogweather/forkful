---
aliases:
- /it/cpp/finding-the-length-of-a-string/
date: 2024-01-20 17:46:51.573612-07:00
description: "Calcolare la lunghezza di una stringa significa scoprire quanti caratteri\
  \ contiene. I programmatori fanno questo per manipolare testo, convalidare input,\u2026"
lastmod: 2024-02-18 23:08:56.164166
model: gpt-4-1106-preview
summary: "Calcolare la lunghezza di una stringa significa scoprire quanti caratteri\
  \ contiene. I programmatori fanno questo per manipolare testo, convalidare input,\u2026"
title: Trovare la lunghezza di una stringa
---

{{< edit_this_page >}}

## What & Why?
Calcolare la lunghezza di una stringa significa scoprire quanti caratteri contiene. I programmatori fanno questo per manipolare testo, convalidare input, o semplicemente per sapere quando sono alla fine.

## How to:
```C++
#include <iostream>
#include <string>

int main() {
    std::string saluto = "Ciao, mondo!";
    
    // Uso la funzione 'length' per ottenere la lunghezza della stringa
    std::cout << "Lunghezza della stringa: " << saluto.length() << std::endl;

    return 0;
}
```

Output:
```
Lunghezza della stringa: 13
```

## Deep Dive
Inizialmente, in C, la lunghezza di una stringa veniva trovata con `strlen`, che conta i caratteri fino al terminatore NULL (`'\0'`). In C++, con la `std::string` class, `length()` e `size()` fanno la stessa cosa e sono più sicuri perché gestiscono la memoria automaticamente.

Le alternative includono:
- Cicli manuali: contare carattere per carattere fino a `'\0'`.
- Funzioni della libreria C come `strlen` (meno sicure).
- Operazioni su range in C++11 o superiore (`std::begin`, `std::end`).

Dettagli sull'implementazione:
- `std::string::size()` è garantito essere di complessità O(1) – veloce.
- Le stringhe in C++ sono oggetti, non solo array, perciò offrono sicurezza e flessibilità aggiuntive.

## See Also
- Standard Template Library (STL): [http://www.cplusplus.com/reference/string/string/](http://www.cplusplus.com/reference/string/string/)
- C++ Reference for `std::string::length`: [https://en.cppreference.com/w/cpp/string/basic_string/length](https://en.cppreference.com/w/cpp/string/basic_string/length)
- C++ Reference for `begin` and `end`: [https://en.cppreference.com/w/cpp/iterator/begin](https://en.cppreference.com/w/cpp/iterator/begin) 
- Learn more about the `strlen` function: [http://www.cplusplus.com/reference/cstring/strlen/](http://www.cplusplus.com/reference/cstring/strlen/)
