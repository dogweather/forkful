---
title:                "Pisanie testów"
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to tworzenie skryptów automatycznie sprawdzających, czy Twój kod działa jak należy. Programiści to robią, żeby szybko wyłapać błędy i spać spokojnie, wiedząc, że wprowadzone zmiany nie zepsują aplikacji.

## Jak to zrobić:
Spójrz na prosty przykład w C++ wykorzystujący framework Catch2:
```C++
#define CATCH_CONFIG_MAIN  // Pozwala Catch2 stworzyć main
#include <catch.hpp>

int dodaj(int a, int b) {
    return a + b;
}

TEST_CASE("Dodawanie działą poprawnie", "[matematyka]") {
    REQUIRE(dodaj(2, 2) == 4);
    REQUIRE(dodaj(-1, 1) == 0);
}
```
Po uruchomieniu zobaczysz:
```
All tests passed (2 assertions in 1 test case)
```
## Głębsze spojrzenie:
Pisanie testów w C++ zaczęło nabierać tempa wraz z popularyzacją Agile i TDD (Test-Driven Development) w latach 2000. Alternatywami dla Catch2 są Google Test czy Boost.Test. Każdy z nich ma swoje zalety, ale Catch2 wyróżnia się prostotą. W implementacji, ważne jest, by testy były izolowane, szybkie i wyraźnie mówiły, co poszło nie tak, gdy zawiodą.

## Zobacz również:
- [Dokumentacja Catch2](https://github.com/catchorg/Catch2)
- [Google Test](https://github.com/google/googletest)
- [Boost.Test](https://www.boost.org/doc/libs/release/libs/test/)