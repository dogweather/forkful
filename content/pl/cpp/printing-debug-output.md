---
title:                "C++: Wydrukowanie wyników debugowania"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista wie, jak ważne jest debugowanie programów. Jedną z przydatnych technik, które pomagają nam w tym procesie, jest drukowanie wyjścia debugowania. W tej krótkiej instrukcji zaprezentujemy, dlaczego jest to przydatne i jak to zrobić w języku C++.

## Jak to zrobić

Do drukowania wyjścia debugowania w C++ używamy funkcji ```cout``` z biblioteki standardowej ```iostream```. Przykładowy kod może wyglądać następująco:

```
#include <iostream>

using namespace std;

int main() {
    int x = 10;
    cout << "Wartość zmiennej x to: " << x << endl;
    return 0;
}
```

Wynikiem działania tego programu będzie wyświetlenie tekstu "Wartość zmiennej x to: 10" w konsoli. Dzięki temu możemy sprawdzić poprawność przypisanych wartości do zmiennych oraz prześledzić działanie programu krok po kroku.

## Deep Dive

Drukowanie wyjścia debugowania może być szczególnie przydatne w przypadku skomplikowanych algorytmów czy błędów, które trudno jest zlokalizować. Dzięki temu, że mamy możliwość wyświetlenia zmiennych w różnych częściach programu, możemy łatwiej zidentyfikować problem oraz zrozumieć, jak wygląda działanie programu na każdym etapie.

Należy jednak pamiętać, że drukowanie za dużo informacji debugowania może wpłynąć negatywnie na wydajność naszego programu. Dlatego ważne jest, aby stosować je z umiarem i usuwać je po zakończeniu procesu debugowania.

## Zobacz również

- [Artykuł na temat debugowania w C++](https://www.geeksforgeeks.org/c-plus-plus/)
- [Oficjalna dokumentacja C++](https://en.cppreference.com/w/)
- [Przykłady kodów z wykorzystaniem funkcji cout](https://www.programiz.com/cpp-programming/output-input)