---
title:                "Elixir: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto czytać argumenty wiersza poleceń? Ponieważ jest to ważny i powszechnie używany element w programowaniu Elixir, który pozwala na interakcję z użytkownikiem i przekazywanie mu informacji w formie parametrów.

## Jak to zrobić

Aby czytać argumenty wiersza poleceń w Elixir, musimy skorzystać z funkcji `System.argv/0`. Przykładowy kod wygląda następująco:

```Elixir
defmodule CommandLine do
  def print_arguments do
    IO.inspect(System.argv())
  end
end

CommandLine.print_arguments()
```

W powyższym kodzie wykorzystujemy funkcję `IO.inspect/1`, która wyświetla nam zawartość listy z argumentami wiersza poleceń. Wywołanie powyższego kodu `elixir command_line.exs argument1 argument2` wyświetli następujący wynik:

```
["argument1", "argument2"]
```

Możemy również przekazać do naszej funkcji pojedynczy argument i wyświetlić jego wartość, na przykład `System.argv(1)` dla pierwszego argumentu.

## Głębszy wgląd

Oprócz wyświetlania argumentów wiersza poleceń, możemy również wykorzystać funkcję `System.argv/1` do przekazania nazwy pliku lub ścieżki dostępu do pliku jako argumentu. Jest to przydatne, jeśli chcemy przetwarzać pliki za pomocą naszych programów Elixir.

Pamiętajmy również, że funkcja `System.argv/0` zwraca listę zawierającą także nazwę pliku uruchamiającego nasz program jako pierwszy element listy.

## Zobacz również

- [Dokumentacja Elixir - System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Poradnik na temat czytania argumentów wiersza poleceń w Elixir](https://www.baeldung.com/elixir-command-line-args)