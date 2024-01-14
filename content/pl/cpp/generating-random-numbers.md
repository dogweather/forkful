---
title:    "C++: Generowanie losowych liczb"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważnym aspektem programowania w C++. Pozwala to na tworzenie dynamicznych i zróżnicowanych aplikacji, które nie są przewidywalne dla użytkowników. Zobaczmy, jak to zrobić!

## Jak To Zrobić

```c++
#include <iostream>
#include <cstdlib> // zawiera funkcję rand()
#include <ctime> // zawiera funkcję srand()

using namespace std;

int main() {

    srand(time(NULL)); // inicjalizuje generator liczb losowych

    // generowanie liczby całkowitej z zakresu od 0 do 99
    int randomNumber = rand() % 100;

    // generowanie liczby rzeczywistej od 0 do 1
    double randomDouble = (double) rand() / RAND_MAX;

    // wyświetlenie wygenerowanych liczb
    cout << "Liczb losowa całkowita: " << randomNumber << endl;
    cout << "Liczba losowa rzeczywista: " << randomDouble << endl;

    return 0;
}
```

Przykładowy wynik:
```
Liczba losowa całkowita: 84
Liczba losowa rzeczywista: 0.497103
```

## Głębsza Analiza

Generator liczb losowych w C++ opiera się na algorytmie o nazwie "Linear Congruential Generator". Polega on na wykorzystaniu pewnej formuły matematycznej, aby wygenerować liczbę pseudolosową. 

Ważnym aspektem używania generatora liczb losowych w C++ jest inicjalizacja funkcji `srand()` za pomocą `time(NULL)`. Dzięki temu generator będzie korzystał z aktualnego czasu jako ziarna, co sprawi, że wygenerowane liczby będą zupełnie różne przy każdym uruchomieniu programu.

Pamiętaj, że wykorzystanie generatora liczb losowych może być przydatne nie tylko w celach rozrywkowych, ale także w przypadku niektórych zadań programistycznych, takich jak sortowanie z wykorzystaniem algorytmu Quicksort.

## Zobacz Również

- Dokumentacja C++ na temat generatora liczb losowych: [http://www.cplusplus.com/reference/cstdlib/rand/](http://www.cplusplus.com/reference/cstdlib/rand/)
- Przykładowe zadania programistyczne wykorzystujące generowanie liczb losowych: [https://www.geeksforgeeks.org/generate-random-number-range-c/](https://www.geeksforgeeks.org/generate-random-number-range-c/)
- Wprowadzenie do algorytmu Quicksort: [https://www.programiz.com/dsa/quick-sort](https://www.programiz.com/dsa/quick-sort)