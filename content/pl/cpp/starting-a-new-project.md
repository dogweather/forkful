---
title:                "Rozpoczynanie nowego projektu"
aliases:
- pl/cpp/starting-a-new-project.md
date:                  2024-01-20T18:03:10.356180-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Rozpoczynanie nowego projektu to jak otwieranie pustej książki — miejsce, gdzie zaczyna się snuć nową opowieść kodowania, wprowadzając pomysły w życie. Programiści biorą się za to, aby rozwiązywać problemy, nauczyć się nowych technologii, lub zrealizować unikatowy pomysł.

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
