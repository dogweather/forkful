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

## Co i dlaczego?

Pisanie testów jest często uciążliwym, ale niezbędnym elementem pracy programisty. Testy polegają na tworzeniu specjalnych fragmentów kodu, które sprawdzają, czy inne części kodu działają poprawnie. Robimy to po to, aby upewnić się, że nasz program działa zgodnie z oczekiwaniami i aby uniknąć błędów w przyszłości.

## Jak to zrobić:

```C++
#include <iostream>

// Przykładowa funkcja, którą będziemy testować
int dodaj(int a, int b) {
  return a + b;
}

int main() {
  // Przykładowe testy dla funkcji dodaj
  std::cout << "Test 1: " << (dodaj(2, 3) == 5 ? "PASSED" : "FAILED") << std::endl;
  std::cout << "Test 2: " << (dodaj(-1, 5) == 4 ? "PASSED" : "FAILED") << std::endl;
  std::cout << "Test 3: " << (dodaj(0, 0) == 0 ? "PASSED" : "FAILED") << std::endl;
  return 0;
}
```

### Przykładowy wynik (output):
```
Test 1: PASSED
Test 2: PASSED
Test 3: PASSED
```

## Głębsze spojrzenie:

Pisanie testów jest stosunkowo nowym podejściem w programowaniu. Do niedawna wyłącznym sposobem sprawdzania poprawności kodu była ręczna weryfikacja przez programistę. Istnieją również inne metody testowania, takie jak testowanie manualne, ale są one mniej efektywne i bardziej podatne na błędy.

Pisanie testów może być zaburzające dla programisty, ponieważ wymaga dodatkowego czasu i wysiłku. Jednak jest to inwestycja, która zwraca się w przyszłości, ponieważ pomaga w unikaniu błędów i ułatwia rozwój i utrzymanie aplikacji.

## Zobacz również:

- [The Importance of Writing Tests in Software Development](https://www.freecodecamp.org/news/what-are-software-tests-and-why-are-they-important/)
- [Benefits of Test-Driven Development](https://dzone.com/articles/test-driven-development-tdd-the-benefits-of-test)