---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:49.051040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Generowanie liczb losowych to proces tworzenia ciągu liczb lub symboli nieprzewidywalnych dla obserwatorów. Programiści wykorzystują to, by dodać element przypadkowości: od gier, przez symulacje, aż po bezpieczeństwo aplikacji.

## How to: (Jak to zrobić?)
```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd; // Inicjalizacja generatora
    std::mt19937 gen(rd()); // Seed generatora z urządzenia losowego
    std::uniform_int_distribution<> distr(1, 100); // Zakres losowania

    // Wygenerowanie i wypisanie losowej liczby
    std::cout << "Losowa liczba: " << distr(gen) << std::endl;

    return 0;
}
```
Sample output (Przykładowy wynik):
```
Losowa liczba: 42
```

## Deep Dive (Dogłębna analiza)
Początki generatorów liczb losowych sięgają teorii i praktyki statystycznej. Pierwotne metody, jak np. rzuty monetą, były niewystarczające dla potrzeb nauki i techniki. W XX wieku wprowadzono algorytmy generujące sekwencje liczb, które wyglądają na losowe, ale są w istocie deterministyczne — tzw. pseudolosowe.

Alternatywy to np. `std::default_random_engine`, który jest mniej złożony niż `std::mt19937`, ale też z reguły mniej nieprzewidywalny. Istnieje też pojęcie true randomness (prawdziwej losowości), którą można uzyskać z zewnętrznych zdarzeń niezwiązanych z programem, lecz jest to rzadziej używane.

Implementacja w C++ korzysta z nowoczesnej biblioteki `<random>`, która oferuje wiele rodzajów generatorów i rozkładów. `std::mt19937` to generator Mersenne Twister zapewniający wysoką jakość liczb pseudolosowych. Używa się go ze "seedem" z `std::random_device`, by zwiększyć nieregularność generowanych sekwencji.

## See Also (Zobacz także)
- C++ Reference on `<random>`: https://en.cppreference.com/w/cpp/header/random
- Tutorial on random numbers by cplusplus.com: http://www.cplusplus.com/reference/random/
- Insights into Mersenne Twister Algorithm: https://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
