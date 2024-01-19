---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb jest procesem tworzenia serii liczb, które nie mają żadnej logicznej sekwencji czy wzorca. Programiści stosują to w różnych celach - od symulacji złożonych scenariuszy, przez tworzenie gier, aż do zabezpieczania danych.

## Jak to zrobić:

Generowanie losowych liczb w C++ jest proste dzięki bibliotece <random>. Oto przykładowe użycie:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;  
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 6);

    for(int n=0; n<10; ++n)
        std::cout << distr(gen) << ' ';
    return 0;
}
```

W tym kodzie generujemy 10 liczb z zakresu 1-6 (jak losowanie kostki do gry).

## W głąb tematu:

Generowanie losowych liczb ma długą historię w informatyce. W starszych wersjach C++, używano funkcji rand() do generowania losowych liczb, ale ta funkcja ma pewne ograniczenia.

Alternatywą dla metody prezentowanej w sekcji "Jak to zrobić" jest użycie narzędzi dostępnych w Boost.Random, które oferują bardziej rozbudowane możliwości.

Przy generowaniu liczby losowej, musimy zawsze brać pod uwagę dwa elementy: generator (np. std::mt19937) oraz dystrybucję (np. std::uniform_int_distribution<>). Generator tworzy ciąg losowych wartości bezpośrednio, a dystrybucja transformuje te wartości tak, aby pasowały do naszych potrzeb.

## Zobacz też:

- Dokumentacja C++ na temat <random>: https://en.cppreference.com/w/cpp/numeric/random
- Porównanie różnych generatorów liczb losowych: https://www.pcg-random.org/other-rngs.html
- Biblioteka Boost.Random: https://www.boost.org/doc/libs/1_75_0/doc/html/boost_random.html