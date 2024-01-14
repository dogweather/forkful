---
title:                "C++: Generowanie losowych liczb."
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego generowanie liczb losowych jest ważne w programowaniu?

Generowanie liczb losowych jest istotnym elementem w świecie programowania. Pozwala ono na tworzenie różnorodnych i nieprzewidywalnych wartości, które mogą być wykorzystane w różnych algorytmach i aplikacjach. Dzięki temu możliwe jest symulowanie różnych scenariuszy i testowanie kodu w różnych warunkach. W tym artykule postaram się wyjaśnić, dlaczego generowanie liczb losowych jest ważne oraz jak z łatwością można to osiągnąć w języku C++.

## Jak to zrobić?

Aby wygenerować losową liczbę w C++, potrzebujemy skorzystać z biblioteki <random>. Poniżej przedstawiam przykładowy kod, który wyświetla 10 liczb losowych z zakresu od 1 do 100:

```C++
#include <iostream>
#include <random>

int main() {

  // ustawienie generatora losowości
  std::random_device rd;
  // wykorzystanie generatora do wygenerowania ziarna
  std::mt19937 generator(rd());
  // ustalenie zakresu liczb
  std::uniform_int_distribution<int> distribution(1, 100);
  // wyświetlenie 10 liczb losowych
  for(int i = 0; i < 10; i++) {
    std::cout << distribution(generator) << " ";
  }

  return 0;
}
```

Przykładowy wynik:

```C++
56 32 98 17 84 21 46 63 75 36
```

W powyższym kodzie wykorzystano funkcję <random_device> do wygenerowania losowego ziarna oraz generatora liczb losowych <mt19937>. Następnie ustalono zakres liczb za pomocą funkcji <uniform_int_distribution> i przy użyciu pętli wypisano 10 losowych wartości.

## Głębszy zanurzenie

W języku C++ istnieje wiele możliwości generowania liczb losowych, w zależności od potrzeb. Można wykorzystać różne rodzaje generatorów, jak również ustalać bardziej skomplikowane parametry, np. rozkłady normalne czy wykładnicze. Wiedza o dostępnych opcjach może być szczególnie przydatna przy tworzeniu złożonych programów i symulacji.

Warto również pamiętać, że pomimo wykorzystania funkcji <random>, generowane liczby mogą nie być w pełni losowe. W takim przypadku warto sięgnąć po inne źródła losowości, np. dane z czujników lub wpisy użytkownika.

## Zobacz także

- [Dokumentacja biblioteki <random> dla języka C++](https://en.cppreference.com/w/cpp/numeric/random)
- [Przewodnik po generowaniu liczb losowych w C++](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Więcej o złożonych generowaniach w języku C++](https://www.geeksforgeeks.org/generating-random-number-range-c/)

Dzięki wykorzystaniu odpowiednich narzędzi i wiedzy o generowaniu liczb losowych, programowanie staje się jeszcze ciekawsze i bardziej wszechstronne. Bawmy się i twórzmy różnorodne aplikacje wykorzystując potęgę losowości!