---
title:                "Łączenie ciągów znaków"
html_title:           "Gleam: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
Większość programów, które są tworzone, muszą manipulować tekstami, np. łącząc różne słowa lub zdania w jedno. Do tego celu wykorzystywana jest operacja konkatenacji - czyli łączenie stringów (ciągów znaków).

Dlaczego programiści tak często korzystają z konkatenacji? Ponieważ pozwala to na tworzenie bardziej złożonych tekstów w prosty sposób. Zamiast ręcznie łączyć poszczególne elementy, można wykorzystać funkcję konkatenacji, która oszczędzi nam czas i wysiłek.

## Jak to zrobić:
Istnieje kilka sposobów na konkatenację stringów, a w Gleam wykorzystujemy operator ```++```, który łączy dwa stringi w jeden. Przykłady użycia:

```Gleam
let str1 = "Hello"
let str2 = "world"
let result = str1 ++ " " ++ str2
IO.println(result)
```

Wynik: ```Hello world```

Możemy również łączyć więcej niż dwa stringi:

```Gleam
let str1 = "This"
let str2 = "is"
let str3 = "a"
let str4 = "sentence."
let result = str1 ++ " " ++ str2 ++ " " ++ str3 ++ " " ++ str4
IO.println(result)
```

Wynik: ```This is a sentence.```

## Głębokie zanurzenie:
Konkatenacja stringów jest powszechnie wykorzystywana w programowaniu, ale pierwotnie została wprowadzona w języku programowania Fortran w latach 50. Alternatywami są np. funkcje formatujące, które pozwalają na bardziej zaawansowane manipulacje stringami.

Jeśli chodzi o implementację w Gleam, operacja konkatenacji jest niezwykle wydajna, ponieważ język ten jest zaprojektowany tak, aby działać szybko i skutecznie z różnymi strukturami danych.

## Zobacz także:
Jeśli chcesz dowiedzieć się więcej o konkatenacji stringów w Gleam, polecamy zapoznanie się z dokumentacją języka oraz przeglądnięcie dostępnych funkcji i operacji, które mogą ułatwić Ci pracę z tekstami.