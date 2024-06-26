---
date: 2024-01-20 17:55:45.927454-07:00
description: "How to: Usare gli argomenti della riga di comando \xE8 semplice. Ecco\
  \ un esempio."
lastmod: '2024-03-13T22:44:43.743379-06:00'
model: gpt-4-1106-preview
summary: "Usare gli argomenti della riga di comando \xE8 semplice."
title: Lettura degli argomenti della riga di comando
weight: 23
---

## How to:
Usare gli argomenti della riga di comando è semplice. Ecco un esempio:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "Hai inserito " << argc - 1 << " argomenti:\n";
    for (int i = 1; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << '\n';
    }
    return 0;
}
```

Se salvi questo come `argomenti.cpp` e lo esegui con `./argomenti uno due tre`, l'output sarà:
```
Hai inserito 3 argomenti:
1: uno
2: due
3: tre
```

## Deep Dive
Nel C++, la funzione `main` può accettare due parametri: `argc` (argument count) e `argv` (argument vector). Questa convenzione risale al C originale. 

Alternative? Potresti usare librerie come `boost::program_options` o `getopt` in ambiente POSIX per gestire opzioni e argomenti più complessi. 

I dettagli di implementazione sono importanti. `argv[0]` è il nome del programma. Gli argomenti vanno da `argv[1]` a `argv[argc-1]`. Ricorda, `argv` è un array di stringhe terminate da null (`nullptr` in C++).

## See Also
- Documentazione C++ su `main` e argomenti della riga di comando: http://www.cplusplus.com/doc/tutorial/program_structure/
- Libreria Boost Program Options: https://www.boost.org/doc/libs/release/libs/program_options/
- Opzioni di riga di comando POSIX `getopt`: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
