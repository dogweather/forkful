---
title:    "C++: Pisanie testów"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodzowną częścią naszego codziennego życia. Aby stworzyć niezawodny i wydajny system, ważne jest, aby wykonywać testy jednostkowe. Pisząc testy, możemy upewnić się, że nasz kod działa poprawnie i jest łatwiejszy w utrzymaniu. Jest to również ważny element procesu programistycznego, który pomaga w wykrywaniu błędów na wczesnym etapie, co może znacznie zmniejszyć koszty naprawy w przyszłości.

## Jak to zrobić

Pisanie testów jednostkowych jest procesem prostym, a dzięki zastosowaniu specjalnych bibliotek, takich jak Google Test czy Catch, możemy jeszcze bardziej ułatwić sobie pracę. Ważne jest, aby testy były napisane w myśl zasad "Arrange - Act - Assert". Poniżej przedstawiono przykładowy kod testu przy użyciu biblioteki Google Test:

```C++
#include <gtest/gtest.h>
#include "program.h"

TEST(ProgramTest, DodajTest) {
  // Arrange
  int a = 5;
  int b = 3;
  
  // Act
  int wynik = dodaj(a, b);
  
  // Assert
  EXPECT_EQ(wynik, 8);
}
```

Zauważ, że najpierw tworzymy zmienne wejściowe, które będą wykorzystane w teście, a następnie wywołujemy funkcję, którą chcemy przetestować. Na końcu używamy asercji, aby sprawdzić, czy otrzymany wynik jest zgodny z oczekiwaniami. Dzięki temu, że testy są zautomatyzowane, możemy je łatwo uruchamiać za każdym razem, gdy wprowadzamy zmiany w naszym kodzie.

## Deep Dive

Pisanie testów jednostkowych wiąże się również z pewnymi wyzwaniami. W przypadku skomplikowanych funkcji, może okazać się trudne przetestowanie wszystkich możliwych kombinacji wejściowych. Dlatego też ważne jest, aby wybierać takie wartości, które sprawdzą poprawność działania funkcji w jak największej ilości scenariuszy.

Ponadto, istnieje wiele rodzajów testów jednostkowych, takich jak testy wydajnościowe czy testy integracyjne, które mogą dodatkowo zwiększyć jakość naszego kodu. Ważne jest, aby dostosować rodzaj testu do konkretnego problemu, aby uzyskać optymalne rezultaty.

## Zobacz również

- [Blog: Jak pisać testy jednostkowe w C++](https://blog.testproject.io/2019/08/28/how-to-write-unit-tests-in-cpp/)
- [Poradnik: Testy jednostkowe w C++ z wykorzystaniem biblioteki Catch](https://www.catch-tests.net/)
- [Kurs: Pisanie efektywnych testów jednostkowych w C++](https://www.udemy.com/course/coding-test-cpp/)