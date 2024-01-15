---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "C++: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś miałby chcieć przeszukiwać lub zmieniać tekst w swoim kodzie? Cóż, istnieje wiele powodów, dla których taka czynność może być potrzebna. Może to być konieczne do naprawienia błędów w kodzie, dostosowania do nowych wymagań lub uproszczenia procesu pisania kodu.

## Jak to zrobić

Szukanie i zmienianie tekstu w kodzie jest możliwe dzięki zastosowaniu funkcji find() i replace() w języku C++. Oto przykładowy kod, który pokazuje, jak wykorzystać te funkcje w swoim programie:

```C++
// Ustawienie stringa, który chcemy przeszukać
string szukanyString = "Hello world!";

// Użyj find() do znalezienia pozycji, od której zacznie się zmiana
size_t pozycja = szukanyString.find("Hello");

// Użyj replace() do zamiany tekstu od określonej pozycji
szukanyString.replace(pozycja, 5, "Hi");

// Wyświetl zmieniony string
cout << szukanyString << endl;
```

Output: "Hi world!"

Mówiąc po ludzku, funkcja find() szuka określonego tekstu w podanym stringu i zwraca pozycję, od której ten tekst się zaczyna. Następnie, funkcja replace() zamienia wybrany tekst zaczynając od określonej pozycji na podany nowy tekst.

## Głębszy zanurkaj

W przypadku szukania i zmieniania tekstu w kodzie, istnieje kilka ważnych rzeczy, o które należy pamiętać. Po pierwsze, funkcja find() zwraca wartość typu size_t, który jest liczbą całkowitą bez znaku, więc jeśli nasza szukana fraza nie jest znaleziona, zwróci ona wartość max_size(), czyli największą możliwą wartość tego typu.

Po drugie, funkcja replace() może przyjąć jako drugi argument również wartość typu size_t, co pozwala nam na zamienianie konkretnych liczb znaków w wybranym tekście.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcjach find() i replace() w języku C++, sprawdź poniższe linki:

1. Dokumentacja C++ dla funkcji find(): [https://pl.cppreference.com/w/cpp/string/basic_string/find](https://pl.cppreference.com/w/cpp/string/basic_string/find)
2. Dokumentacja C++ dla funkcji replace() : [https://pl.cppreference.com/w/cpp/string/basic_string/replace](https://pl.cppreference.com/w/cpp/string/basic_string/replace)
3. Przykładowy tutorial na temat szukania i zamiany tekstu w C++: [https://www.programiz.com/cpp-programming/string-replace](https://www.programiz.com/cpp-programming/string-replace)

Teraz, gdy już wiesz, jak działa szukanie i zmienianie tekstu w C++, możesz śmiało zacząć wykorzystywać te funkcje w swoim kodzie. Powodzenia!