---
title:                "Tworzenie losowych liczb"
html_title:           "C++: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cześć programiści!

Dzisiaj zajmiemy się tematem generowania liczb losowych w języku C++. 
## Co i dlaczego?

Generowanie liczb losowych jest ważnym aspektem wielu aplikacji i gier. Polega to na losowym wybieraniu liczb z określonego zakresu. Programiści wykorzystują to, aby urozmaicić swoje programy, symulować przypadkowe zdarzenia lub tworzyć unikalne klucze dostępu. 

## Jak to zrobić:

Możemy użyć funkcji `rand()` w C++, która generuje liczby całkowite w zakresie od 0 do `RAND_MAX` - domyślnie 32767. Możemy określić również własny zakres, używając prostej operacji arytmetycznej i zapisując wynik do zmiennej. 

```C++
int losowaLiczba = rand() % 100; // wygeneruje liczbę od 0 do 99 
int zakres = 100; 
int liczba = rand() % zakres + 1; // wygeneruje liczbę od 1 do 100 
```

Podczas uruchamiania programu wygenerowane liczby będą różne - dzięki temu mamy zapewnioną losowość. 

## Pełniejsza analiza:

Losowa liczba komputerowa jest wygenerowana za pomocą algorytmu pseudolosowego, który powtarza się w stałym rytmie. Przez lata powstało wiele algorytmów generowania liczb losowych, jednak nie jest możliwe uzyskanie idealnie losowych liczb za pomocą komputera. Alternatywą dla funkcji `rand()` jest użycie funkcji bibliotecznej `random`, która oferuje bardziej zaawansowane sposoby na generowanie liczb losowych. 

## Zobacz też:

Jeśli chcesz zgłębić temat generowania liczb losowych w C++, polecamy przeczytać poniższe źródła:

- [Dokumentacja funkcji rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Tutorial o funkcji random w C++](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Inne algorytmy generowania liczb losowych](https://en.wikipedia.org/wiki/List_of_random_number_generators)

Dziękuję za uwagę i rozpocznijcie losowanie!