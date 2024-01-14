---
title:    "Elixir: Odczytywanie argumentów wiersza poleceń"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Dlaczego warto spróbować czytać argumenty wiersza poleceń w Elixirze?

Czytanie i przetwarzanie argumentów wiersza poleceń może być niezwykle przydatnym umiejętnością dla programistów w Elixirze, ponieważ pozwala na interakcję z naszym programem za pomocą argumentów podanych podczas uruchamiania. Pozwala to na dostosowanie i personalizację naszych programów, a także na ustawienie pewnych opcji i flag w zależności od potrzeb.

# Jak to zrobić?

Czytanie argumentów wiersza poleceń w Elixirze jest bardzo łatwe i sprowadza się do użycia funkcji `System.argv/1`, która zwraca listę argumentów przekazanych przy uruchamianiu naszego programu. W poniższym przykładzie wykorzystamy tę funkcję i wyświetlimy na ekranie każdy podany argument:

```Elixir
defmodule CommandArgs do
  def print_args() do
    args = System.argv()
    Enum.each(args, fn arg ->
      IO.puts("Podano argument: #{arg}")
    end)
  end
end

CommandArgs.print_args()
```

Po uruchomieniu programu z argumentami, na przykład `elixir command_args.exs argument1 argument2`, otrzymamy następujący wynik:

```bash
Podano argument: argument1
Podano argument: argument2
```

Możemy także przetwarzać i wykorzystywać poszczególne argumenty w naszym programie, na przykład korzystając z funkcji `List.first/1` i `List.last/1`:

```Elixir
# przykładowa funkcja, która odczytuje dwa argumenty i zwraca ich sumę
defmodule AddArgs do
  def add_args() do
    arg1 = System.argv() |> List.first() |> String.to_integer()
    arg2 = System.argv() |> List.last() |> String.to_integer()
    arg1 + arg2
  end
end

IO.puts("Wynik dodawania: #{AddArgs.add_args()}")
```

# W głębi tematu

Podczas czytania argumentów wiersza poleceń warto wiedzieć, że funkcja `System.argv/1` zwraca listę napisów (stringów) i należy pamiętać, aby dopasować typ danych odpowiednio do potrzeb. Warto także zwrócić uwagę na obecność argumentów specjalnych, takich jak opcje z prefiksem `-` lub flagi z prefiksem `--`, które mogą mieć różne znaczenia i być wykorzystywane przez różne programy. Możemy także wykorzystać bibliotekę `OptionParser`, aby upewnić się, że nasz program odczyta i przetworzy argumenty poprawnie.

# Zobacz także

- [Dokumentacja Elixir - System.argv/1](https://hexdocs.pm/elixir/System.html#argv/1)
- [Biblioteka OptionParser](https://hexdocs.pm/OptionParser/OptionParser.html)