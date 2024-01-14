---
title:    "C++: Pisanie testów"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne dla każdego programisty?

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki nim możemy upewnić się, że nasz kod działa poprawnie i sprawdzić czy wprowadzone zmiany nie wpłynęły negatywnie na działanie aplikacji. Testowanie jest również wykorzystywane w celu zapobiegania błędom i ułatwienia debugowania. W tym wpisie dowiesz się jak pisać testy w języku C++.

## Jak pisać testy w języku C++?

W pierwszym kroku musisz zainstalować odpowiedni framework do testowania, tak jak np. Google Test lub Catch. Następnie należy stworzyć plik z testami, w którym będziemy przetestować działanie funkcji. W poniższym przykładzie użyjemy frameworku Google Test.

```C++
#include <gtest/gtest.h> // dołączamy bibliotekę Google Test

// funkcja, którą będziemy testować - zwraca kwadrat podanej liczby
int square(int num) {
    return num * num;
}

// przykładowy test z wykorzystaniem biblioteki Google Test
TEST(SquareTest, NumberIs10) { 
    EXPECT_EQ(square(10), 100); // sprawdzamy czy zwracana wartość jest równa oczekiwanej
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv); // inicjalizacja frameworku
    return RUN_ALL_TESTS(); // uruchomienie testów
}
```

Powyższy przykład ilustruje jak wygląda pisanie testów w języku C++. Pamiętaj, że każda funkcja powinna mieć przypisany co najmniej jeden test, a wszystkie możliwe przypadki powinny zostać przetestowane. W ten sposób możesz mieć pewność, że Twoja aplikacja jest wolna od błędów.

## Głębszy wgląd w pisanie testów

Pisanie testów nie tylko pomaga w weryfikacji poprawności działania kodu, ale również pozwala na tworzenie lepszego i bardziej czytelnego kodu. Testy są również bardzo pomocne podczas wprowadzania zmian w istniejącym kodzie, ponieważ pozwalają na szybkie sprawdzenie czy zmiany nie wpłynęły negatywnie na pozostałą część aplikacji. Dodatkowo, testy mogą być wykorzystane do dokumentowania funkcjonalności i zachowania kodu.

## Zobacz także

- [Poradnik do frameworku Google Test](https://github.com/google/googletest/blob/master/googletest/docs/primer.md)
- [Poradnik do frameworku Catch](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
- [Artykuł na temat TDD (Test Driven Development)](https://dev.to/mikhailsharkov/writing-c-tests-with-google-test-and-bazel-workshop-7on)
- [Poradnik do tworzenia testów jednostkowych w języku C++](https://www.tutorialspoint.com/cplusplus/cpp_testing.htm)

Naucz się pisać testy i zobacz jak wiele korzyści może przynieść to Twojemu kodowi i procesowi tworzenia oprogramowania. Warto poświęcić czas na naukę i stosowanie tej praktyki w swoich projektach. Zapewni to większą pewność i jakość Twojego kodu.