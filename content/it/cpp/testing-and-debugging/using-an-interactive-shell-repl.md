---
date: 2024-01-26 04:12:14.262463-07:00
description: "Un REPL (Read-Eval-Print-Loop, Ciclo Leggi-Valuta-Stampa) \xE8 un ambiente\
  \ di programmazione semplice e interattivo. I programmatori lo utilizzano per\u2026"
lastmod: '2024-03-11T00:14:17.345516-06:00'
model: gpt-4-0125-preview
summary: "Un REPL (Read-Eval-Print-Loop, Ciclo Leggi-Valuta-Stampa) \xE8 un ambiente\
  \ di programmazione semplice e interattivo. I programmatori lo utilizzano per\u2026"
title: Utilizzo di un interprete interattivo (REPL)
---

{{< edit_this_page >}}

## Cosa e perché?
Un REPL (Read-Eval-Print-Loop, Ciclo Leggi-Valuta-Stampa) è un ambiente di programmazione semplice e interattivo. I programmatori lo utilizzano per sperimentare in tempo reale con il linguaggio, per compiti rapidi o per comprendere nuovi concetti senza l'onere di creare applicazioni complete.

## Come fare:
C++ non viene fornito con un REPL integrato, ma strumenti come Cling offrono questa capacità. Ecco come utilizzare Cling per calcolare la somma di due numeri:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "La somma è: " << a + b << std::endl;
    return 0;
}

// Output:
// La somma è: 12
```

Avvia Cling e inserisci il codice linea per linea, osservando l'output dopo ogni comando. È un feedback immediato, senza compilazione.

## Approfondimento
I REPL sono comuni per linguaggi come Python o Lisp e sono presenti dagli anni '60. Per C++, un linguaggio compilato, il concetto non si adatta così naturalmente, ed è per questo che esistono strumenti come Cling: interpretano C++ al volo. Le alternative includono compilatori online o piccoli programmi di test compilati tradizionalmente. Cling è costruito sulla base di LLVM e Clang, fornendo un ponte affinché C++ possa essere utilizzato in modo interpretato.

## Vedi anche
- [Cling](https://root.cern/cling/): Un interprete interattivo di C++, basato sulle librerie LLVM e Clang.
- [Jupyter Notebooks](https://jupyter.org/): Offre un guscio interattivo all'interno di un ambiente di notebook, supporta C++ attraverso il kernel xeus-cling.
- [LLVM](https://llvm.org/): Una collezione di tecnologie di compilatori e toolkit modulari e riutilizzabili, su cui si basa Cling.
