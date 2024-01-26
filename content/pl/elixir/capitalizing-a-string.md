---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Kapitalizacja stringu to proces przekształcania pierwszych liter w wyrazach na wielkie litery. Programiści robią to dla lepszego wyglądu, szczególnie przy nazwach własnych, tytułach lub w interfejsach użytkownika.

## Jak to zrobić:
```elixir
defmodule StringHelper do
  def capitalize_string(str) do
    str
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

# Przykładowe wywołanie:
IO.puts StringHelper.capitalize_string("elixir jest fajny") 
# Wynik: "Elixir Jest Fajny"
```

## Deep Dive
Kapitalizowanie stringów to nie nowość; funkcję taką znajdziemy w wielu językach programowania. W Elixirze używamy `String.capitalize/1` by zrobić pierwszą literę wielką. Pozostałe języki mają swoje odpowiedniki, np. `capitalize` w Ruby czy `toTitleCase` w Java.

Implementacja w Elixirze jest prosta dzięki potokowi funkcji (`|>`) oraz modułom `Enum` i `String`. Możemy też użyć rekurencji lub wzorców dopasowania dla własnych rozwiązań, gdyby podstawowa funkcja `String.capitalize/1` nie spełniała naszych potrzeb.

## See Also
- [Elixir String Documentation](https://hexdocs.pm/elixir/String.html)
- [Enum Module in Elixir](https://hexdocs.pm/elixir/Enum.html)
- [Kapitalizacja w innych językach programowania](https://rosettacode.org/wiki/Letter_frequency#Capitalize_the_first_letter_of_each_word)
