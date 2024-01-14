---
title:                "C++: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości łańcucha tekstowego jest częstym zadaniem w programowaniu. Pozwala to na określenie ilości znaków w danym łańcuchu, co jest bardzo przydatne podczas przetwarzania danych lub wyświetlania informacji użytkownikowi. W tym artykule dowiesz się, jak to zrobić i dlaczego jest to ważne.

## Jak to zrobić

Aby znaleźć długość łańcucha w języku C++, możesz skorzystać z funkcji `length()` lub `size()`. Obie zwracają liczbę znaków w podanym łańcuchu. Przykładowy kod wykorzystujący funkcję `length()` wyglądałby tak:

```C++
#include <iostream>
#include <string>

int main() {
  std::string name = "Adam";
  int length = name.length();
  std::cout << "Długość łańcucha imienia " << name << " wynosi " << length << " znaków.";
  return 0;
}
```

**Output:** Długość łańcucha imienia Adam wynosi 4 znaki.

Możesz również wykorzystać pętlę `for` i metodę `size()` do przejrzenia każdego znaku w łańcuchu i zliczenia ich. Przykładowy kod wykorzystujący tę metodę wyglądałby tak:

```C++
#include <iostream>
#include <string>

int main() {
  std::string word = "Kot";
  int length = 0;
  for(int i = 0; i < word.size(); i++) {
    length++;
  }
  std::cout << "Długość łańcucha " << word << " wynosi " << length << " znaków.";
  return 0;
}
```

**Output:** Długość łańcucha Kot wynosi 3 znaki.

## Deep Dive

W języku C++, łańcuchy tekstowe są przechowywane jako tablice znaków, której ostatnim elementem jest zero oznaczające koniec łańcucha. Z tego powodu funkcje `length()` i `size()` zwracają liczbę znaków w łańcuchu bez uwzględnienia znaku końca. Dzięki temu zabiegowi, możesz wykorzystywać pętle i funkcje na łańcuchach, w podobny sposób, jak na zwykłych tablicach.

Gdy używasz metody `size()` w pętli, musisz pamiętać, że indeksowanie w języku C++ zaczyna się od 0, dlatego umieszczenie warunku zwraca liczbę, o jeden większą niż rzeczywista długość łańcucha. Dlatego wykorzystujemy pętlę z warunkiem `i < word.size()` zamiast `i <= word.size()`.

## Zobacz również

- [Funkcja `length()` w C++ na cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [Metoda `size()` w C++ na cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/size)