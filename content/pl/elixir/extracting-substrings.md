---
title:                "Elixir: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się potrzebować wyodrębnić część tekstu z większego ciągu znaków? W takich sytuacjach, funkcja wyodrębniająca substring będzie Twoim najlepszym przyjacielem. W tym artykule dowiesz się, dlaczego warto nauczyć się korzystać z tej funkcji w języku Elixir.

## Jak to zrobić

Aby wyodrębnić substring z tekstu, możesz użyć wbudowanej funkcji `String.slice/3`. W pierwszym argumencie podajemy tekst, z którego chcemy wyodrębnić substring, w drugim określamy początkowy indeks, a w trzecim końcowy indeks. Przykładowy kod wyglądałby następująco:

```Elixir
text = "Hej, to jest przykładowy tekst"
subtext = String.slice(text, 7, 15)
IO.puts(subtext) #=> "to jest"
```

Możemy również podać ujemne indeksy, co oznacza liczenie od końca tekstu. Na przykład, `-1` oznacza ostatni znak, `-2` przedostatni itd. Przykładowy kod wyglądałby następująco:

```Elixir
text = "Hej, to jest przykładowy tekst"
subtext = String.slice(text, 7, -5)
IO.puts(subtext) #=> "to jest przykładowy"
```

Jeśli nie podamy trzeciego argumentu, funkcja `String.slice/3` wyodrębni tekst od podanego indeksu do końca tekstu. Natomiast jeśli podamy tylko pierwszy argument, zostanie zwrócony cały tekst. Przykładowy kod wyglądałby następująco:

```Elixir
text = "Hej, to jest przykładowy tekst"
subtext = String.slice(text, 7)
IO.puts(subtext) #=> "to jest przykładowy tekst"
```

## Głębsze zagłębienie

Funkcja `String.slice/3` jest często wykorzystywana do przetwarzania tekstu w aplikacjach internetowych, np. do wyświetlania krótszych wersji tytułów lub opisów artykułów. Możemy również wykorzystać ją do prostego filtrowania tekstu czy też do sprawdzania zawartości konkretnych znaków.

## Zobacz także

- [Dokumentacja Elixir](https://hexdocs.pm/elixir/String.html#slice/3)
- [Przykłady zastosowań slices](https://dev.to/franzejr/using-elixir-s-string-slice-3-function-5666)