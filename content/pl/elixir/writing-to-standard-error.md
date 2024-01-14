---
title:    "Elixir: Pisanie do standardowego błędu"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Zapis do standardowego błędu (standard error) jest ważnym elementem programowania w Elixir. Pozwala on na wyświetlenie błędów i informacji diagnostycznych w sposób niezależny od standardowego wyjścia (standard output), co jest szczególnie przydatne w przypadku niektórych problemów związanych z wyjątkami. 

## Jak to zrobić

Aby zapisać do standardowego błędu w Elixir, wystarczy użyć funkcji `IO.write/2` lub `IO.puts/2`. Poniżej znajdują się przykłady kodu wraz z odpowiadającym im wyjściem:

```elixir
IO.write(:stderr, "To jest przykładowy błąd.")
```

```
To jest przykładowy błąd.
```

```elixir
IO.puts(:stderr, "Ta linijka zostanie wyświetlona na standardowym błędzie.")
```

```
Ta linijka zostanie wyświetlona na standardowym błędzie.
```

## Wielkie zanurzenie

Zapis do standardowego błędu może być użyteczny nie tylko w przypadku wyjątków, ale również do śledzenia działania programu i diagnostyki problemów. Może być on również stosowany w celach edukacyjnych, aby wyjaśnić określone koncepty lub sposób działania kodu. 

Możliwe jest również przekierowanie wyjścia standardowego do pliku za pomocą komendy `exec`, na przykład:

```elixir
exec ["elixir", "my_script.exs"], stderr: "log.txt"
```

Jest to szczególnie użyteczne w przypadku skryptów, które wymagają regularnego zapisywania wyjścia lub błędów do pliku.

## Zobacz także

- Dokumentacja funkcji `IO.write/2` i `IO.puts/2`: [https://hexdocs.pm/elixir/IO.html#write/2](https://hexdocs.pm/elixir/IO.html#write/2)
- Przekierowywanie wyjścia w Elixir: [https://elixir-lang.org/getting-started/basic-usage.html#redirecting-inputoutput](https://elixir-lang.org/getting-started/basic-usage.html#redirecting-input-output)