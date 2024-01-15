---
title:                "Wypisywanie wyników debugowania"
html_title:           "C++: Wypisywanie wyników debugowania"
simple_title:         "Wypisywanie wyników debugowania"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego
Debugowanie to nieodzowny proces w tworzeniu oprogramowania. Dzięki wypisywaniu informacji o stanie programu możemy łatwiej znaleźć i naprawić błędy. W tym artykule dowiesz się, dlaczego warto używać drukowania danych debugowania w języku C++.

## Jak to zrobić
W języku C++ do wypisywania danych debugowania używamy funkcji ```cout```. W przypadku aplikacji konsolowych możemy wypisywać dane bezpośrednio na standardowe wyjście. Na przykład:
```C++
cout << "Zmienna x ma wartość: " << x << endl;
```
W przypadku aplikacji graficznych, gdzie nie mamy dostępu do konsoli, możemy zapisywać dane do pliku dziennika lub wyświetlać je w oknie dialogowym.

## Zagłębianie się
Możemy różnie formatować dane wyjściowe, na przykład za pomocą flag formatujących lub manipulatorów strumienia. Dobrą praktyką jest również dodawanie informacji o miejscu, w którym wypisujemy dane (nazwa funkcji, linia kodu). Dzięki temu łatwiej zlokalizujemy miejsce, w którym wystąpił błąd. Pamiętaj jednak, aby usuwać wszystkie wypisywanie danych debugowania po zakończeniu procesu debugowania, aby nie spowolnić działania programu.

## Zobacz także
- [Dokumentacja języka C++ o standardowym strumieniu wyjścia](http://www.cplusplus.com/reference/iostream/cout/)
- [Artykuł o debugowaniu w języku C++](https://www.ibm.com/developerworks/library/l-debug.html)
- [Poradnik o wypisywaniu danych debugowania w języku C++](https://www.geeksforgeeks.org/printing-output-of-c-program-set-1-cout/)