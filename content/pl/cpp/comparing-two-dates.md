---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest niezbędnym etapem w wielu programach, szczególnie w przypadku aplikacji biznesowych lub systemów przetwarzania danych. Przydatne jest zatem znać efektywny sposób porównywania dat w języku C++.

## Jak to zrobić

Porównywanie dwóch dat w C++ jest możliwe dzięki użyciu klasy `std::chrono::system_clock`. Jest to klasa biblioteki standardowej C++, która służy do różnych operacji związanych z odczytywaniem i manipulowaniem czasem.

Aby porównać dwie daty, należy najpierw utworzyć obiekty `std::chrono::system_clock::time_point` dla każdej z dat, a następnie wykorzystać operator porównania (`<` lub `>`) do porównania tych obiektów. Na przykład:

```C++
#include <chrono>
#include <iostream>

int main()
{
  auto d1 = std::chrono::system_clock::now(); // bieżąca data i godzina
  auto d2 = d1 + std::chrono::hours(24); // data i godzina dnia jutrzejszego

  if(d1 < d2)
  {
    std::cout << "Dzisiaj jest wcześniej niż jutro!" << std::endl;
  }
  else
  {
    std::cout << "Dzisiaj jest później lub taka sama data co jutro." << std::endl;
  }

  return 0;
}
```

W powyższym przykładzie najpierw tworzymy obiekt `d1` zawierający bieżącą datę i godzinę. Następnie tworzymy obiekt `d2`, który jest datą i godziną dnia jutrzejszego, dodając do `d1` obiekt `std::chrono::hours(24)` (co odpowiada 24 godzinom). Kolejnym krokiem jest porównanie obu obiektów za pomocą operatora `<` i wyświetlenie odpowiedniego komunikatu w zależności od wyniku porównania.

Warto zauważyć, że obiekty `time_point` można także przekonwertować na liczbę sekund lub milisekund od początku epoki (1 stycznia 1970 roku), dzięki czemu jest możliwe ich dokładniejsze porównywanie. Można to zrobić za pomocą funkcji `std::chrono::system_clock::to_time_t()` lub `std::chrono::system_clock::to_time_t()`.

## Deep Dive

Klasa `std::chrono::system_clock` używa systemowej implementacji czasu, co oznacza, że wyniki operacji zależą od ustawień systemowych, takich jak strefa czasowa. W niektórych przypadkach może to prowadzić do nieoczekiwanego zachowania porównywania dat.

Ponadto, klasy `std::chrono` oferują również inne sposoby na operacje na czasie, takie jak porównywanie z dokładnością do milisekund (`std::chrono::steady_clock`), pomiar czasu wykonywania kodu (`std::chrono::high_resolution_clock`) oraz wyznaczanie precyzyjnych momentów w czasie (`std::chrono::system_clock`).

## Zobacz także

- Dokumentacja C++ dla klasy `std::chrono::system_clock`: https://en.cppreference.com/w/cpp/chrono/system_clock
- Porównywanie dat w innych językach programowania: https://www.oreilly.com/library/view/data-wrangling-with/9781491948811/ch04.html