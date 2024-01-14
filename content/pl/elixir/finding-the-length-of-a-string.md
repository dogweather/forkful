---
title:                "Elixir: Znajdowanie długości ciągu znaków"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotyka się potrzebę sprawdzenia długości łańcucha znaków. Może to być przydatne, na przykład, przy tworzeniu formularzy lub sprawdzaniu poprawności wprowadzonych danych.

## Jak to zrobić

Można użyć metody `String.length` do znalezienia długości łańcucha. Przykładowy kod wyglądałby następująco:

```elixir
string = "Cześć!"
String.length(string)
```

To zwróci wartość `6`, ponieważ łańcuch składa się z 6 znaków. W przypadku pustego łańcucha, metoda `String.length` zwróci wartość `0`.

## Nurkowanie w głąb

Istnieje kilka istotnych faktów, o których warto wiedzieć przy używaniu metody `String.length`. Po pierwsze, ta metoda może być wywoływana na dowolnym typie danych, który implementuje protokół `String.Chars`. Oznacza to, że możemy używać jej nie tylko na łańcuchach, ale również na innych typach danych, takich jak liczby czy listy. 

Ponadto, `String.length` nie liczy znaków w sposób tradycyjny, ale zwraca liczbę kodową każdego znaku. Oznacza to, że niektóre znaki Unicode mogą zostać policzone jako więcej niż jeden znak. Jednak dla większości przypadków ten sposób liczenia jest wystarczający.

## Zobacz również

- Dokumentacja Elixir dla metody `String.length`: https://hexdocs.pm/elixir/String.html#length/1