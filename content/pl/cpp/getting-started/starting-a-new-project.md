---
date: 2024-01-20 18:03:10.356180-07:00
description: "How to: (Jak to zrobi\u0107:) Uruchomienie nowego programu w C++ historycznie\
  \ rzecz bior\u0105c kiedy\u015B wymaga\u0142o wi\u0119cej kodu. Wczesne kompilatory\
  \ i systemy mog\u0142y\u2026"
lastmod: '2024-04-05T22:50:50.048627-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Uruchomienie nowego programu w C++ historycznie rzecz\
  \ bior\u0105c kiedy\u015B wymaga\u0142o wi\u0119cej kodu."
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to: (Jak to zrobić:)
```C++
#include <iostream>

int main() {
    std::cout << "Hello, New Project!" << std::endl;
    return 0;
}
```
Sample output:
```
Hello, New Project!
```
To prosty początek. Teraz stwórz folder, zapisz kod w pliku `main.cpp`, użyj kompilatora (np. g++), by stworzyć plik wykonywalny, a potem... uruchom go!

## Deep Dive (Głębsze spojrzenie)
Uruchomienie nowego programu w C++ historycznie rzecz biorąc kiedyś wymagało więcej kodu. Wczesne kompilatory i systemy mogły wymagać deklaracji funkcji głównych inaczej niż teraz. Obecnie, z `#include <iostream>` i standardową funkcją `main()`, jesteśmy w stanie szybko rozpocząć pracę.

Inne środowiska jak .NET czy Java mają swoje własne "Hello World" konwencje, ale w C++ ważne jest zrozumienie zarządzania plikami źródłowymi i kompilacji.

C++ ewoluuje. Nowe standardy, jak C++20, wprowadzają udogodnienia (np. coroutines, moduły), które mogą zmienić sposób, w jaki inicjujemy projekty. Oczywiście, do tworzenia dużych aplikacji używamy narzędzi takich jak IDE (Integrated Development Environment), systemy budowania jak CMake, i menedżery pakietów, np. Conan. Ale zrozumienie, jak to zrobić "od zera", jest kluczowe.

## See Also (Zobacz też)
- [cppreference.com](https://en.cppreference.com/w/) - dokumentacja C++
- [learncpp.com](https://www.learncpp.com/) - samouczki C++
- [cmake.org](https://cmake.org/) - o CMake, narzędziu budowania projektów
- [conan.io](https://conan.io/) - o Conan, systemie zarządzania pakietami w C++
