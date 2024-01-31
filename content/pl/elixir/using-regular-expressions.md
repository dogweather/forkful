---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Używanie wyrażeń regularnych (regexów) służy do wyszukiwania i manipulowania tekstami według określonych wzorców. Programiści używają ich do walidacji danych, przeszukiwania logów, refactoringu kodu i wielu innych zadań wymagających elastycznej obsługi tekstu.

## How to:
W Elixirze operujemy na regexach przy pomocy modułu `Regex`. Przykłady użycia:

```elixir
# Znalezienie wszystkich wystąpień
regex = ~r/hello/
str = "hello world, hello elixir"
matches = Regex.scan(regex, str)
IO.inspect(matches) # => [["hello"], ["hello"]]

# Zamiana tekstu
str = "foobar"
new_str = Regex.replace(~r/bar/, str, "baz")
IO.puts(new_str) # => "foobaz"

# Sprawdzanie dopasowania
if Regex.match?(~r/world/, "hello world") do
  IO.puts("Znaleziono!") # => "Znaleziono!"
end
```

## Deep Dive
Wyrażenia regularne mają korzenie w automatach i teorii języka formalnego z lat 50. Elixir wykorzystuje bibliotekę `:re`, która jest oparta na regular expressions engine BEAM (maszyna wirtualna Erlanga). Alternatywą dla regexów są funkcje jak `String.contains?/2` czy parsery takie jak leex i yecc, ale często regexy są wygodniejsze i szybsze do prostych zadań.

## See Also
- [Dokumentacja Regex modułu Elixir](https://hexdocs.pm/elixir/Regex.html)
