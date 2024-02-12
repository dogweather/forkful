---
title:                "Pisanie testów"
aliases:
- pl/cpp/writing-tests.md
date:                  2024-02-03T19:30:06.853562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w C++ polega na tworzeniu małych, samodzielnych programów, które automatycznie weryfikują zachowanie fragmentów twojej bazy kodu. Programiści robią to, aby upewnić się, że ich kod działa zgodnie z oczekiwaniami, aby zapobiec regresji (tj. nowe zmiany psujące istniejącą funkcjonalność) oraz aby ułatwić utrzymanie baz kodów w czasie.

## Jak to zrobić:

### Korzystając z Google Test Framework

Jedną z najpopularniejszych bibliotek firm trzecich do pisania testów w C++ jest Google Test. Najpierw musisz zainstalować Google Test i połączyć go z twoim projektem. Po skonfigurowaniu możesz zacząć pisać przypadki testowe.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Zapisz kod w pliku i skompiluj go przy użyciu kompilatora g++, łącząc bibliotekę Google Test. Jeśli wszystko jest poprawnie skonfigurowane, uruchomienie wynikowego pliku wykonywalnego uruchomi test, a jeśli funkcja `add` działa zgodnie z oczekiwaniami, zobaczysz coś takiego:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Korzystając z Catch2

Innym popularnym frameworkiem do testowania dla C++ jest Catch2. Posiada prostszą składnię i zazwyczaj nie wymaga łączenia z biblioteką (tylko nagłówek). Oto przykład, jak napisać prosty test z Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // To mówi Catch, aby dostarczył main() - rób to tylko w jednym pliku cpp
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Liczby całkowite są mnożone", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Po skompilowaniu i uruchomieniu tego testu, Catch2 zapewnia jasne informacje wskazujące, czy test zakończył się sukcesem, czy niepowodzeniem, wraz z wszelkimi informacjami potrzebnymi do debugowania błędów:

```
===============================================================================
Wszystkie testy zaliczone (1 asercja w 1 przypadku testowym)
```

Te przykłady pokazują, jak integracja frameworków testowych z twoim procesem rozwoju w C++ może znacząco zwiększyć niezawodność i możliwość utrzymania twojego kodu.
