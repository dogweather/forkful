---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C++: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w trakcie pisania programów musimy dokonać usuwania znaków, które pasują do określonego wzoru. Jest to szczególnie przydatne w przypadku przetwarzania ciągów znaków, takich jak adresy email czy numery telefonów. Zapoznaj się z poniższym artykułem, aby dowiedzieć się, dlaczego i jak można to zrobić w języku C++.

## Jak to zrobić

Usuwanie znaków o określonym wzorcu w języku C++ jest stosunkowo proste dzięki wykorzystaniu funkcji bibliotecznej `std::remove_if()`. Przykładowy kod wykorzystujący tę funkcję może wyglądać następująco (zakładając, że wcześniej zostały zdefiniowane odpowiednie zmienne i wektory):

```C++
std::remove_if(wektor.begin(), wektor.end(), [](char c) { return c == 'a' || c == 'b'; });
```

Powyższy kod usunie z wektora wszystkie wystąpienia znaków 'a' oraz 'b'. Poniżej znajduje się pełny przykład kodu wraz z wynikiem działania:

```C++
#include <iostream>
#include <vector>
#include <algorithm>

int main()
{
    std::vector<char> wektor{'a', 'b', 'c', 'd', 'e', 'f'};

    std::cout << "Przed usunięciem: ";
    for (auto c : wektor) {
        std::cout << c << " ";
    }

    std::cout << std::endl;

    std::remove_if(wektor.begin(), wektor.end(), [](char c) { return c == 'a' || c == 'b'; });

    std::cout << "Po usunięciu: ";
    for (auto c : wektor) {
        std::cout << c << " ";
    }

    return 0;
}
```

```
// Wynik działania programu:
Przed usunięciem: a b c d e f
Po usunięciu: c d e f
```

## Deep Dive

Funkcja `std::remove_if()` działa poprzez przesuwanie wszystkich elementów spełniających warunek na koniec wektora i zwraca iterator wskazujący na pierwszy element, który nie spełnia warunku. Następnie, przy pomocy metody `erase()` można usunąć wszystkie elementy znajdujące się za tym iteratorem. Liczba elementów usuniętych przez funkcję `std::remove_if()` jest zwracana jako wartość zwracana i może być wykorzystana do aktualizacji rozmiaru wektora (np. przy pomocy metody `resize()`).

## Zobacz także

- Dokumentacja funkcji `std::remove_if()` w języku C++
- Przykłady usuwania znaków o określonym wzorcu w innych językach programowania: [Python](https://www.geeksforgeeks.org/python-remove-all-values-from-a-list-present-in-other-list/) [JavaScript](https://www.geeksforgeeks.org/javascript-remove-all-the-elements-that-matches-the-given-regular-expression/)