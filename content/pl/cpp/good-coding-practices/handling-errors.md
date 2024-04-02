---
date: 2024-01-26 00:49:40.335055-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w to planowanie na wypadek, gdy co\u015B\
  \ p\xF3jdzie nie tak. Jest to kluczowe, poniewa\u017C pomaga unikn\u0105\u0107 awarii\
  \ i sprawia, \u017Ce oprogramowanie jest\u2026"
lastmod: '2024-03-13T22:44:35.720343-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w to planowanie na wypadek, gdy co\u015B\
  \ p\xF3jdzie nie tak. Jest to kluczowe, poniewa\u017C pomaga unikn\u0105\u0107 awarii\
  \ i sprawia, \u017Ce oprogramowanie jest\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Co i dlaczego?
Obsługa błędów to planowanie na wypadek, gdy coś pójdzie nie tak. Jest to kluczowe, ponieważ pomaga uniknąć awarii i sprawia, że oprogramowanie jest solidne i przyjazne dla użytkownika.

## Jak to zrobić:
Oto podstawowy blok try-catch służący do obsługi wyjątku:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Ups! Coś poszło nie tak.");
    } catch (const std::exception& e) {
        std::cerr << "Błąd: " << e.what() << std::endl;
    }
    return 0;
}
```

Przykładowy wynik:
```
Błąd: Ups! Coś poszło nie tak.
```

## Wgłębienie się w temat
C++ obsługuje błędy od swoich wczesnych dni. Najbardziej podstawową formą była kontrola wartości zwracanych. Jeśli masz już doświadczenie, pamiętasz dni przed-standardowe: C z klasami i ręczną kontrolą błędów.

Następnie przyszły wyjątki w C++, które dały nam uporządkowany sposób radzenia sobie z nieoczekiwanymi problemami. Wyjątek jest zgłaszany za pomocą `throw` i przechwytywany dzięki `try/catch`.

Często pojawiają się dwa typy błędów: logiczne, jak błędne obliczenie, oraz błędy czasu wykonywania, jak dostęp do nieprawidłowego adresu pamięci. Wyjątki są idealne dla błędów czasu wykonywania. Dla błędów logicznych często lepiej jest użyć asercji lub kodów błędów.

Trwa ciągła debata na temat wyjątków kontra kody błędów. Wyjątki mogą być wolniejsze i mogą prowadzić do skomplikowanego przepływu sterowania. Kody błędów, choć szybsze, mogą sprawić, że kod będzie zagracony i trudniejszy do utrzymania. To kompromis, więc kluczowe jest zrozumienie swojego przypadku użycia.

C++17 wprowadził `std::optional` i `std::variant`, które są alternatywami dla wyjątków. Są one przydatne w funkcjach, które mogą, ale nie muszą zwrócić poprawnego wyniku.

Bezpieczeństwo wyjątków to kolejny ból głowy. Chodzi o gwarancje, które twój kod zapewnia pomimo wyjątków. Mamy trzy poziomy: podstawowy, silny i nothrow. Im więcej gwarancji, tym bardziej złożony może być twój kod.

Ostatnie przemyślenia — obsługa błędów to tak samo sztuka, jak nauka. Kształtuje to, jak twoja aplikacja przetrwa w świecie rzeczywistym. Nie nadużywaj wyjątków. Dąż do czytelnego, łatwego do utrzymania kodu.

## Zobacz również
- [cppreference o obsłudze wyjątków](https://en.cppreference.com/w/cpp/language/exceptions)
- [Stanowisko Bjarne'a Stroustrupa na temat obsługi błędów](http://www.stroustrup.com/except.pdf)
- [Wytyczne C++ Core na temat wyjątków](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
