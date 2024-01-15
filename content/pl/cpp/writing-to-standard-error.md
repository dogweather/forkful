---
title:                "Pisanie do standardowego błędu"
html_title:           "C++: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli zajmujesz się programowaniem w języku C++, prawdopodobnie wielokrotnie spotkałeś się z potrzebą wypisywania informacji na standardowe wyjście błędu. Jest to przydatne w celu raportowania błędów lub ostrzeżeń, które mogą się pojawić w trakcie działania programu. W tym artykule dowiesz się, dlaczego i w jaki sposób można to zrobić.

## Jak to zrobić

Aby wypisać informację na standardowe wyjście błędu, możemy skorzystać z funkcji `std::cerr` z biblioteki standardowej C++. Oto przykładowe użycie tej funkcji:

```C++
#include <iostream>

int main() {
    std::cerr << "To jest informacja na standardowe wyjście błędu" << std::endl;
    return 0;
}
```

Powyższy kod wypisze na standardowe wyjście błędu tekst "To jest informacja na standardowe wyjście błędu". Warto zauważyć, że do wypisywania na standardowe wyjście błędu używamy specjalnego strumienia `std::cerr`, a nie standardowego strumienia `std::cout`.

Możemy również wykorzystać operator `<<` do wypisywania różnych typów danych, na przykład:

```C++
#include <iostream>

int main() {
    int liczba = 123;
    std::cerr << "Liczba to: " << liczba << std::endl;
    return 0;
}
```

Wyjściem powyższego kodu będzie tekst "Liczba to: 123".

## Wnikliwy rzut oka

Podczas wypisywania na standardowe wyjście błędu, warto zwrócić uwagę na kilka rzeczy. Po pierwsze, warto pamiętać, że strumień `std::cerr` jest buforyzowany, co oznacza, że tekst może nie być od razu wypisywany, a dopiero po wywołaniu funkcji `std::endl` lub `std::flush`.

Kolejną ważną kwestią jest możliwość przekierowania standardowego wyjścia błędu np. do pliku lub innego urządzenia. Możemy to zrobić wykorzystując funkcję `freopen` z biblioteki `cstdio`. Poniżej przedstawiam przykład przekierowania standardowego wyjścia błędu do pliku "error.txt":

```C++
#include <iostream>
#include <cstdio>

int main() {
    freopen("error.txt", "w", stderr); // przekierowanie do pliku error.txt
    std::cerr << "To będzie wypisane do pliku error.txt" << std::endl;
    return 0;
}
```

Warto również zauważyć, że w niektórych przypadkach mogą pojawić się problemy z wypisywaniem na standardowe wyjście błędu, gdy program kończy się niepowodzeniem (np. zwraca kod błędu). Jednym ze sposobów na uniknięcie tego problemu jest wykorzystanie funkcji `std::abort()` z biblioteki standardowej C++, która powoduje natychmiastowe zakończenie programu bez wywoływania destruktora.

## Zobacz także

- [Dokumentacja funkcji std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [Poradnik C++ - strumienie wejścia i wyjścia](https://pl.wikibooks.org/wiki/C%2B%2B/Programowanie_proceduralne/Strumienie_wej%C5%9Bcia_i_wyj%C5%9Bcia)
- [Przekierowanie wyjścia błędu do pliku w C++](https://stackoverflow.com/questions/10075404/capture-and-log-all-stderr-and-stdout-to-a-file-while-still-being-able-to-view)