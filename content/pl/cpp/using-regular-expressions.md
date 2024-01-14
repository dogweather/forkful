---
title:    "C++: Używanie wyrażeń regularnych"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia są niezbędnym narzędziem dla każdego programisty, który chce manipulować i przetwarzać tekst w swoim kodzie. Pozwalają one na szybkie i skuteczne wyszukanie i manipulację tekstową, co jest szczególnie przydatne przy obsłudze większych zbiorów danych.

## Jak to zrobić

Aby użyć regularnych wyrażeń w języku C++, należy najpierw zaimportować bibliotekę wyrażeń regularnych (```#include <regex>```). Następnie należy utworzyć obiekt typu ```regex``` zawierający wyrażenie, którego poszukujemy, oraz wywołać funkcję ```regex_search()``` na przeszukiwanym tekście. Przykładowy kod wyglądać może tak:

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string text = "Przykładowy tekst do przeszukania.";
    regex expression("tekst");

    if (regex_search(text, expression)) {
        cout << "Wyrażenie zostało znalezione w tekście." << endl;
    } else {
        cout << "Wyrażenie nie zostało znalezione w tekście." << endl;
    }

    return 0;
}
```

Dla powyższego kodu, oczekiwanym wyjściem jest "Wyrażenie zostało znalezione w tekście." Warto zauważyć, że regularne wyrażenia są bardzo elastyczne i umożliwiają zastosowanie wielu zaawansowanych funkcji, takich jak grupowanie i wyrażenia warunkowe.

## Głębszy wgląd

Regularne wyrażenia są wykorzystywane nie tylko w języku C++, ale również w innych językach programowania oraz narzędziach do przetwarzania tekstu. Dzięki temu, nauka ich jest nie tylko przydatna dla programistów, ale również osób pracujących z dużymi zbiorami danych, takich jak analitycy.

## Zobacz także

- [Dokumentacja biblioteki wyrażeń regularnych w języku C++](https://en.cppreference.com/w/cpp/regex)
- [Poradnik o regularnych wyrażeniach w języku C++](https://www.regular-expressions.info/cpp.html)