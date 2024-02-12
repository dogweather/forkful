---
title:                "Generowanie liczb losowych"
date:                  2024-01-27T20:32:56.925414-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych w programowaniu polega na tworzeniu sekwencji liczb, które nie wykazują żadnego przewidywalnego porządku lub wzoru. Programiści często wykorzystują te liczby do różnych celów, takich jak symulowanie nieprzewidywalnych wydarzeń, w testowaniu i debugowaniu, oraz w algorytmach gier, aby zapewnić sprawiedliwość lub nieprzewidywalność.

## Jak to zrobić:

Aby wygenerować liczby losowe w C++, zazwyczaj korzysta się z nagłówka `<random>`, który został wprowadzony w C++11, oferując szeroki zakres możliwości generowania liczb losowych z różnych rozkładów.

```C++
#include <iostream>
#include <random>

int main() {
    // Inicjalizacja generatora liczb losowych
    std::random_device rd;
    std::mt19937 gen(rd());

    // Definiowanie zakresu [0, 99] włącznie
    std::uniform_int_distribution<> distrib(0, 99);

    // Generowanie i drukowanie 5 losowych liczb w zdefiniowanym zakresie
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Przykładowy kod inicjalizuje generator liczb losowych Mersenne Twister z ziarnem z `std::random_device`. Następnie definiuje jednolity rozkład liczb całkowitych w zakresie [0, 99] i w końcu drukuje 5 losowych liczb z tego rozkładu.

Przykładowe wyjście może wyglądać tak, ale pamiętaj, że każde wykonanie prawdopodobnie przyniesie inne wyniki:

```
45 67 32 23 88
```

## Dogłębna analiza:

Historycznie, generacja liczb losowych w C++ opierała się głównie na funkcji `rand()` oraz funkcji `srand()` do inicjowania ziarna, znajdujących się w nagłówku `<cstdlib>`. Jednakże, to podejście często spotykało się z krytyką za jego brak jednolitości i przewidywalności w rozkładzie generowanych liczb.

Wprowadzenie nagłówka `<random>` w C++11 oznaczało znaczącą poprawę, oferując zaawansowany system do produkcji liczb losowych. Dostarczone udogodnienia obejmują różnorodność silników (takich jak `std::mt19937` dla Mersenne Twister) i rozkładów (takich jak `std::uniform_int_distribution` dla jednolitego rozkładu liczb całkowitych), które mogą być łączone w celu dopasowania do konkretnych potrzeb programisty, prowadząc do bardziej przewidywalnego zachowania, lepszej wydajności i większej elastyczności.

Chociaż biblioteka `<random>` jest znacznie lepsza niż starsze podejście `rand()`, warto zauważyć, że generowanie naprawdę losowych liczb — szczególnie do celów kryptograficznych — nadal opiera się na dodatkowych rozważaniach. Dla aplikacji kryptograficznych powinny być używane biblioteki zaprojektowane specjalnie do celów bezpieczeństwa, które często wykorzystują źródła entropii sprzętowej.
