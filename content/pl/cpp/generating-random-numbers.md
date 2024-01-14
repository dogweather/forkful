---
title:                "C++: Generowanie losowych liczb."
simple_title:         "Generowanie losowych liczb."
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego generowanie losowych liczb jest ważne?

Generowanie losowych liczb jest ważne w wielu przypadkach programowania. Często potrzebujemy wybrać losowo elementy z listy lub stworzyć symulację losowego zdarzenia. Z tego powodu, umiejętność generowania losowych liczb jest bardzo przydatna w codziennych zastosowaniach programowania.

## Jak to zrobić?

Aby wygenerować losową liczbę w C++, używamy funkcji `rand()`. Przykładowy kod wyglądałby następująco:

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main(){
    // ustawianie ziarna dla generatora liczb pseudolosowych
    // w C++11 można użyć std::random_device jako ziarna
    srand(time(0));
    
    // wybieranie losowej liczby od 1 do 100
    int random = rand() % 100 + 1;
    // wybieranie losowej liczby typu double
    double random_double = static_cast<double>(rand()) / RAND_MAX;
    
    // wypisywanie wygenerowanych liczb na ekran
    std::cout << "Wylosowana liczba to: " << random << std::endl;
    std::cout << "Wylosowana liczba typu double to: " << random_double << std::endl;
    
    return 0;
}
```

Przykładowy wynik wygenerowanego kodu może wyglądać następująco:

```
Wylosowana liczba to: 42
Wylosowana liczba typu double to: 0.84252
```

W powyższym kodzie użyliśmy funkcji `srand()` w celu ustawienia ziarna dla generatora liczb pseudolosowych. Dzięki temu za każdym razem, gdy uruchamiamy program, otrzymujemy nowe wartości. Aby uniknąć wygenerowania tych samych liczb przy każdym uruchomieniu programu, warto użyć aktualnego czasu jako ziarna, jak pokazano w powyższym przykładzie.

## Głębszy przegląd

Warto zauważyć, że funkcja `rand()` zwraca liczbę całkowitą z przedziału od 0 do wartości `RAND_MAX` zdefiniowanej w bibliotece standardowej. W powyższym przykładzie użyliśmy tej wartości do wygenerowania losowej liczby typu `double`, poprzez podzielenie wyniku przez `RAND_MAX` oraz przy użyciu rzutowania na typ double.

Ponadto, warto pamiętać, że generowanie liczb pseudolosowych nie jest całkowicie losowym procesem, a raczej wykorzystuje specjalne algorytmy do wygenerowania ciągu liczb, który wydaje się być losowy. Dlatego też, jeśli potrzebujemy większej losowości, możemy rozważyć użycie zewnętrznych bibliotek lub funkcji dostępnych w danym języku programowania.

## Zobacz również

- [Przykładowy kod generujący losowe liczby w C++](https://www.tutorialspoint.com/generate-random-numbers-in-cplusplus)
- [Dokumentacja funkcji rand() w standardzie C++](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [Losowe liczby w programowaniu: czy zawsze są one naprawdę losowe?](https://medium.com/@Zaccc123/pseudo-random-numbers-vs-true-random-numbers-d75d18248a7a)