---
title:                "Pisanie testów"
html_title:           "C++: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie kodu jest nieodłączną częścią procesu programowania. Pomaga zapewnić, że nasz kod działa prawidłowo i jest odporny na błędy. Pisząc testy, możemy również szybko wykryć i naprawić potencjalne problemy w kodzie.

## Jak to zrobić

Aby napisać testy w C++, musimy skorzystać z biblioteki do testowania. Jednym z najpopularniejszych wyborów jest biblioteka Google Test. Zobaczmy przykładowy test sprawdzający poprawność dodawania dwóch liczb całkowitych:

```C++
#include <gtest/gtest.h> // dołączanie biblioteki do projektu

// definicja testu
TEST(AddTest, Positive) {
  // przygotowanie testowych danych
  int x = 5;
  int y = 10;

  // wywołanie funkcji do przetestowania
  int result = x + y;

  // sprawdzenie poprawności wyniku
  ASSERT_EQ(result, 15);
}

// uruchomienie testów
int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

W powyższym przykładzie tworzymy test o nazwie "AddTest", który sprawdza poprawność dodawania dwóch liczb. Korzystając z funkcji "ASSERT_EQ", porównujemy wynik działania naszej funkcji z oczekiwanym rezultatem. Jeśli wyniki się nie zgadzają, test zostanie zakończony niepowodzeniem.

## Deep Dive

Pisanie testów jest częścią podejścia zwinnego do programowania, ponieważ pomaga nam w szybkim wykrywaniu i naprawianiu błędów. Gdy testy są napisane i działają poprawnie, mamy większą pewność, że zmiany wprowadzane w kodzie nie powodują nieoczekiwanych efektów ubocznych. Dzięki temu nasz kod jest bardziej niezawodny i łatwiej go rozwijać.

## Zobacz także

- [Dokumentacja biblioteki Google Test](https://github.com/google/googletest)
- [Tutorial o pisaniu testów w C++](https://www.youtube.com/watch?v=zYK6pJJipno)
- [Książka "Test Driven Development for Embedded C"](https://pragprog.com/book/jgade/test-driven-development-for-embedded-c)