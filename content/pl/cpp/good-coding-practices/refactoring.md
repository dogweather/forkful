---
title:                "Refaktoryzacja"
aliases:
- /pl/cpp/refactoring.md
date:                  2024-01-26T01:17:35.291549-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/refactoring.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Refaktoryzacja to proces zmiany wewnętrznej struktury programu komputerowego bez zmiany jego zewnętrznego zachowania. Programiści robią to, aby oczyścić swój kod, co sprawia, że staje się on łatwiejszy do zrozumienia, utrzymania i rozszerzenia.

## Jak to zrobić:

Wyobraź sobie, że masz funkcję, która robi nieco za dużo, jak ta niezgrabna metoda, która inicjalizuje obiekt i jednocześnie wykonuje rejestrowanie:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Logika inicjalizacji
        // ...

        // Szczegółowe rejestrowanie
        if (verbose) {
            std::cout << "Widget zainicjalizowany!" << std::endl;
        }
    }
};

// Użycie:
Widget w;
w.init(true);
```

Wyjście:
```
Widget zainicjalizowany!
```

Refaktoryzacja tego na czystsze, bardziej skoncentrowane metody może wyglądać tak:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Tylko logika inicjalizacji
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget zainicjalizowany!" << std::endl;
    }
};

// Użycie:
Widget w;
w.init();
w.logInitialization();
```

Ta zmiana nie zmieniła tego, co program robi, ale sprawia, że klasa `Widget` jest bardziej modułowa, a jej użycie bardziej jasne.

## Dogłębna analiza

Pojęcie refaktoryzacji, jakie znamy dzisiaj, ma swoje korzenie w społecznościach programistycznych Smalltalk lat 80-tych i zostało mocno spopularyzowane przez książkę Martina Fowlera "Refaktoryzacja. Ulepszanie struktury istniejącego kodu" z 1999 roku. Dzisiaj refaktoryzacja jest podstawowym elementem nowoczesnego rozwoju oprogramowania, włączonym do różnych metodologii rozwoju, takich jak Agile i TDD (Test-Driven Development).

Gdy mówimy o alternatywach dla refaktoryzacji, wchodzimy na teren przepisywania lub przeprojektowywania. Refaktoryzacja jest strategią i wykonana etapami, podczas gdy przepisanie może odrzucić istniejący kod na rzecz nowego rozwiązania. Przeprojektowanie natomiast może wiązać się z bardziej znaczącymi zmianami, w tym ze zmianą funkcjonalności, co nie jest celem czystej refaktoryzacji.

Szczegóły implementacji dotyczące refaktoryzacji mogą być dość szczegółowe. Istnieje wiele "zapachów kodu", które mogą skłonić do refaktoryzacji, takie jak długie metody, duże klasy czy zduplikowany kod. Istnieją narzędzia automatyczne, które mogą pomóc w refaktoryzacji, takie jak "Clang-Tidy" dla C++, które mogą wykrywać problemy, a nawet stosować pewne poprawki.

Ponadto refaktoryzacja wymaga solidnego zestawu testów, aby zapewnić, że funkcjonalność pozostaje niezmieniona. Bez testów, w zasadzie działasz w ciemno, ryzykując regresję.

## Zobacz także

Aby lepiej zrozumieć refaktoryzację i zobaczyć więcej przykładów, warto sprawdzić:

- Klasyczny tekst Martina Fowlera "Refaktoryzacja. Ulepszanie struktury istniejącego kodu" dla podstawowych pomysłów i strategii.
- Dokumentację `Clang-Tidy` na https://clang.llvm.org/extra/clang-tidy/ dla automatycznego wsparcia w refaktoryzacji w C++.
- "Working Effectively with Legacy Code" Michaela Feathersa, który przedstawia techniki bezpiecznej refaktoryzacji w kontekście nieidealnych istniejących baz kodów.
