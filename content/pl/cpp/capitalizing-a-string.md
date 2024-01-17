---
title:                "Zmiana pierwszej litery w ciągu znaków"
html_title:           "C++: Zmiana pierwszej litery w ciągu znaków"
simple_title:         "Zmiana pierwszej litery w ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Cześć programiści! Dzisiaj porozmawiamy o jednej ważnej funkcji w C++, a mianowicie o "capitalizing a string" (czyli wielkopisanie łańcucha znaków). Zobaczymy, co to takiego i dlaczego warto znać tę funkcję.

## Co i Dlaczego?
Wielkopisanie łańcucha znaków to proces zmiany wszystkich liter w danym tekście na wielkie litery. Programiści często używają tej funkcji, aby ułatwić sobie pracę z tekstem, szczególnie gdy chodzi o porównywanie czy dwie zmienne są takie same. Ponadto, wielkopisanie może być użyteczne w sytuacji, gdy chcemy wyświetlić tekst w jednolitym formacie, np. w nagłówkach w aplikacji.

## Jak to zrobić:
Aby wielkopisać łańcuch znaków w C++, musimy użyć funkcji ```toupper()```, która zamienia małą literę na odpowiadającą jej wielką. Przykładowy kod wyglądałby następująco:
```
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main()
{
    string text = "witaj swiecie!";
    transform(text.begin(), text.end(), text.begin(), ::toupper);
    cout << text << endl;

    return 0;
} 
```
Wynik wyświetli nam: WITAJ SWIECIE!

## Głębsza dyskusja:
Wielkopisanie w programowaniu pojawiło się wraz z pojawieniem się języków programowania. W niektórych językach, takich jak Fortran czy COBOL, wielkość liter nie miała znaczenia, a więc taki proces nie był potrzebny. Jednak w językach takich jak C czy C++, wielkość liter ma znaczenie, dlatego też taka funkcja jest ważna. Większość języków programowania posiada wbudowane funkcje do wielkopisania, ale w niektórych przypadkach warto napisać własną wersję, np. gdy chcemy uwzględnić polskie znaki.

## Zobacz też:
Jeśli chcesz poznać inne przydatne funkcje związane z obsługą tekstu w C++, warto zajrzeć na stronę: https://en.cppreference.com/w/cpp/string/basic_string. Znajdziesz tam wiele przydatnych informacji, włącznie z wielkopisaniem tekstu.