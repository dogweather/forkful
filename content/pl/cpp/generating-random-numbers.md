---
title:                "Generowanie losowych liczb"
html_title:           "C++: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego
Losowe liczby są często używane w programowaniu do symulowania różnych sytuacji lub do generowania unikalnych danych. Dzięki nim można również utworzyć gry lub aplikacje, które są mniej przewidywalne, co może sprawić, że użytkownicy poczują się bardziej zainteresowani.

## Jak to zrobić
Pierwszym krokiem do generowania losowych liczb w C++ jest zaimportowanie biblioteki "cstdlib", która zawiera funkcję "rand". Przykładowy kod wyglądałby w ten sposób:

```C++
#include <cstdlib>

int main() {
    // ustawienie ziarna dla funkcji rand
    srand(time(0));
    
    // wygenerowanie liczby całkowitej z przedziału 0-10
    int random_number = rand() % 11;
    
    // wyświetlenie wyniku
    std::cout << "Wylosowana liczba to: " << random_number << std::endl;
    
    return 0;
}
```

Ten kod najpierw ustawia ziarno dla funkcji "rand" na podstawie aktualnego czasu, aby uzyskać bardziej losowe wyniki. Następnie generuje liczbę całkowitą z przedziału 0-10 za pomocą funkcji "rand" oraz operatora modulo. Na końcu wynik jest wyświetlany za pomocą strumienia "cout".

Jedną z zalet korzystania z biblioteki "cstdlib" jest możliwość generowania również liczb zmiennoprzecinkowych za pomocą funkcji "double rand ()". Jest to przydatne, jeśli potrzebujemy bardziej precyzyjnych wyników, na przykład w przypadku symulacji finansowych lub statystyk.

## Deep Dive
Proces generowania losowych liczb w C++ jest oparty na algorytmie Linear Congruential Generator. Polega on na wykonywaniu operacji matematycznych na bieżącym ziarnie, aby wygenerować kolejne liczby. Dlatego ważne jest, aby ziarno było jak najbardziej losowe, co jest zapewnione poprzez ustawienie go na podstawie aktualnego czasu. W przeciwnym razie, jeśli ziarno będzie stałe, wyniki mogą być powtarzalne.

W przypadku potrzeby wygenerowania dużej liczby losowych liczb, można również zastosować bardziej zaawansowane algorytmy, takie jak Mersenne Twister lub Xorshift. Jednak w większości przypadków funkcja "rand" z biblioteki "cstdlib" jest wystarczająca.

## Zobacz również
- [Dokumentacja "cstdlib" w języku C++](https://www.cplusplus.com/reference/cstdlib/)
- [Wykorzystanie losowych liczb w symulacjach komputerowych](https://en.wikipedia.org/wiki/Random_number_generation#Simulation)
- [Wydajność różnych algorytmów generowania losowych liczb](https://cs.stackexchange.com/questions/1846/time-complexity-for-random-number-generation)