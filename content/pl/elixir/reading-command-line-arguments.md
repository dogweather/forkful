---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie argumentów z linii poleceń to sposób na dostarczenie danych wejściowych do programów z poziomu konsoli. Dzięki temu, programiści mają możliwość manipulowania działaniem programu bez potrzeby modyfikowania jego kodu.

## Jak to zrobić:

Elixir pozwala na odczytywanie argumentów z linii poleceń za pomocą wbudowanej funkcji `System.argv/0`. 

```elixir
defmodule MyProgram do
  def main do
    IO.inspect(System.argv())
  end
end

MyProgram.main
```

Przykładowe użycie wygląda wtedy tak: `elixir my_program.exs arg1 arg2 arg3`. W efekcie dostaniemy `["arg1", "arg2", "arg3"]`.


## Deep Dive

Historia funkcji `System.argv/0` jest dość prosta - została dodana do języka Elixir od samego początku jego istnienia jako sposób na interakcje z konsolą.

Jednym z alternatywnych rozwiązań jest użycie biblioteki `OptionParser`, która jest bardziej rozbudowana i pozwala na stosowanie różnego rodzaju opcji.

Wszystko to działa na bazie funkcji `:init.get_plain_arguments/0` z języka Erlang, na którym budowany jest Elixir. To jest przede wszystkim przykład jak Elixir daje programistom dostęp do niskopoziomowych funkcji Erlanga.

## Zobacz także:

- [Dokumentacja Elixir o `System.argv/0`](https://hexdocs.pm/elixir/System.html#argv/0)
- [Dokumentacja Elixir o `OptionParser`](https://hexdocs.pm/elixir/OptionParser.html)
- [Dokumentacja Erlanga na temat `:init.get_plain_arguments/0`](http://erlang.org/doc/man/init.html#get_plain_arguments-0)