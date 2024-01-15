---
title:                "Znajdowanie długości łańcucha znaków"
html_title:           "Elixir: Znajdowanie długości łańcucha znaków"
simple_title:         "Znajdowanie długości łańcucha znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Na pierwszy rzut oka, znajdowanie długości ciągu znaków może wydawać się nieistotnym zadaniem. Jednak w rzeczywistości jest to często wykorzystywany aspekt programowania, szczególnie przy pracy z tekstem i manipulacji ciągami znaków. Pozwala nam to na dokładne przetwarzanie i analizę danych tekstowych.

## Jak to zrobić
W języku Elixir, istnieje kilka sposobów na znalezienie długości ciągu znaków. Możemy wykorzystać funkcję `String.length/1`, która przyjmuje jeden argument – ciąg znaków – i zwraca jego długość. Przykładowy kod wyglądałby tak:

```Elixir
string = "Hello, world!"
length = String.length(string)

IO.puts "Długość ciągu znaków: #{length}"
```

W tym przypadku, rezultatem wywołania funkcji `String.length/1` jest liczba 13, ponieważ tyle znaków zawiera nasz ciąg. Istnieje również możliwość wykorzystania funkcji `length/1`, która działa na dowolnym typie sekwencyjnym, w tym również na ciągach znaków. Przykładowy kod wyglądałby tak:

```Elixir
string = "Hello, world!"
length = length(string)

IO.puts "Długość ciągu znaków: #{length}"
```

W tym przypadku, rezultatem wywołania funkcji `length/1` jest również liczba 13. Możemy również wykorzystać operator `<>` do konkatenacji kilku ciągów znaków i na końcu wywołać funkcję `length/1`. Przykładowy kod wyglądałby tak:

```Elixir
string1 = "Hello"
string2 = ", "
string3 = "world!"

length = length(string1 <> string2 <> string3)

IO.puts "Długość ciągu znaków: #{length}"
```

W tym przypadku, zmienne `string1`, `string2` i `string3` są połączone w jeden ciąg znaków "Hello, world!", którego długość wynosi 13.

## Głębsze zagadnienia
W języku Elixir, ciągi znaków są traktowane jako listy pojedynczych znaków, dlatego też możemy wykorzystać do nich funkcje związane z listami. Na przykład, jeśli chcemy znaleźć długość ciągu pomijając znak spacji, możemy wykorzystać funkcję `Enum.count/2` w połączeniu z funkcją `String.graphemes/1` do zwrócenia listy pojedynczych znaków obecnych w ciągu. Przykładowy kod wyglądałby tak:

```Elixir
string = "Hello, world!"
length = Enum.count(String.graphemes(string), fn char -> char != " " end)

IO.puts "Długość ciągu znaków (bez spacji): #{length}"
```

W tym przypadku, rezultatem wywołania funkcji `Enum.count/2` jest liczba 11, ponieważ dwie spacje zostały pominięte podczas liczenia.

## Zobacz też
- [Dokumentacja Elixir - Funkcja String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Dokumentacja Elixir - Funkcja length/1](https://hexdocs.pm/elixir/Kernel.html#length/1)
- [Dokumentacja Elixir - Funkcja Enum.count/2](https://hexdocs.pm/elixir/Enum.html#count/2)
- [Dokumentacja Elixir - Funkcja String.graphemes/1](https://hexdocs.pm/elixir/String.html#graphemes/1)