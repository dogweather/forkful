---
title:                "Wyświetlanie informacji debugujących"
html_title:           "Elixir: Wyświetlanie informacji debugujących"
simple_title:         "Wyświetlanie informacji debugujących"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu Elixir, może się zdarzyć, że napotkasz problem lub błąd, który może być trudny do zidentyfikowania. Wtedy przydatne może być debugowanie poprzez wyświetlanie danych na ekranie. Ten artykuł daje wskazówki, jak można to zrobić w prosty sposób.

## Jak to zrobić

Aby wyświetlić dane debugowania w Twoim kodzie Elixir, możesz używać kilku przydatnych funkcji i makr. Poniżej znajdują się przykładowe wykorzystania, wraz z wyjaśnieniem, kiedy i dlaczego powinieneś używać danej metody.

```elixir
# Użycie funkcji IO.inspect
IO.inspect("Hello world")
# Output: "Hello world"

# Użycie makra IO.puts
IO.puts("Hello world")
# Output: Hello world

# Używanie debugowania warunkowego z wykorzystaniem if
if debug?, do: IO.inspect("Debug output")
# Output (tylko jeśli debug? jest prawdziwe): Debug output
```

## Głębsze zanurzenie

Po zapoznaniu się z podstawowymi funkcjami i makrami do wyświetlania danych debugowania, warto dowiedzieć się więcej o tym, jak można dostosować wyświetlane informacje. Oto kilka wskazówek:

- Używaj funkcji IO.inspect tylko w celach debugowania. Nie powinna ona być wykorzystywana w kodzie produkcyjnym.
- Wykorzystuj argument `label: "Nazwa"` w funkcji IO.inspect, aby określić jasną etykietę dla wyświetlanych danych.
- Zastanów się nad wykorzystaniem funkcji IO.inspect z argumentem `pretty: true`, aby otrzymać bardziej czytelną prezentację złożonych danych.

## Zobacz także

- [Oficjalna dokumentacja Elixir o debugowaniu](https://elixir-lang.org/getting-started/debugging.html)
- [Artykuł "Top 10 Tips on How to Debug Elixir Code Effectively"](https://www.zenrows.com/blog/top-10-tips-debug-elixir-code/)