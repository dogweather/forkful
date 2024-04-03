---
date: 2024-01-26 04:12:11.188552-07:00
description: "Jak to zrobi\u0107: C++ nie posiada wbudowanego REPL, ale narz\u0119\
  dzia takie jak Cling oferuj\u0105 t\u0119 mo\u017Cliwo\u015B\u0107. Oto jak u\u017C\
  ywa\u0107 Clinga do obliczenia sumy dw\xF3ch liczb."
lastmod: '2024-03-13T22:44:35.714397-06:00'
model: gpt-4-0125-preview
summary: "C++ nie posiada wbudowanego REPL, ale narz\u0119dzia takie jak Cling oferuj\u0105\
  \ t\u0119 mo\u017Cliwo\u015B\u0107."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
C++ nie posiada wbudowanego REPL, ale narzędzia takie jak Cling oferują tę możliwość. Oto jak używać Clinga do obliczenia sumy dwóch liczb:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "Suma wynosi: " << a + b << std::endl;
    return 0;
}

// Wynik:
// Suma wynosi: 12
```

Uruchom Clinga i wprowadzaj kod linia po linii, obserwując wynik po każdym poleceniu. To natychmiastowa informacja zwrotna, bez kompilacji.

## Dogłębna analiza
REPL są powszechne dla języków takich jak Python czy Lisp i istnieją od lat 60. Dla C++, języka kompilowanego, koncepcja ta nie pasuje tak naturalnie, dlatego istnieją narzędzia takie jak Cling — interpretują C++ na bieżąco. Alternatywami są kompilatory online lub małe programy testowe kompilowane tradycyjnie. Cling jest zbudowany na bazie LLVM i Clanga, zapewniając most dla C++, aby można było go używać w sposób interpretowany.

## Zobacz też
- [Cling](https://root.cern/cling/): Interaktywny interpreter C++, zbudowany na bazie bibliotek LLVM i Clang.
- [Notatniki Jupytera](https://jupyter.org/): Oferują interaktywną powłokę w środowisku notatnika, obsługują C++ poprzez jądro xeus-cling.
- [LLVM](https://llvm.org/): Zbiór modułowych i wielokrotnego użytku technologii kompilatora i narzędzi, na których Cling się opiera.
