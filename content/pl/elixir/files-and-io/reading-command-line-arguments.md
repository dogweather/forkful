---
date: 2024-01-20 17:55:48.556948-07:00
description: "How to (Jak to zrobi\u0107?) Oto jak w Elixirze mo\u017Cemy odczyta\u0107\
  \ argumenty linii polece\u0144."
lastmod: '2024-04-05T22:37:43.763777-06:00'
model: gpt-4-1106-preview
summary: "How to (Jak to zrobi\u0107?) Oto jak w Elixirze mo\u017Cemy odczyta\u0107\
  \ argumenty linii polece\u0144."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to (Jak to zrobić?)
Oto jak w Elixirze możemy odczytać argumenty linii poleceń:

```elixir
defmodule CliArgs do
  def main(args) do
    case args do
      [] -> 
        IO.puts "Nie podano argumentów!"
      _ -> 
        IO.puts "Oto twoje argumenty:"
        Enum.each(args, &IO.puts/1)
    end
  end
end

CliArgs.main(System.argv())
```

Uruchamiając program tak:
```shell
elixir my_script.exs arg1 arg2 arg3
```

Dostaniesz wynik:
```
Oto twoje argumenty:
arg1
arg2
arg3
```

## Deep Dive (Dogłębna analiza)
Argumenty linii poleceń są tak stare jak same systemy operacyjne. W Elixirze odczytujemy je funkcją `System.argv()`, która zwraca listę argumentów jako stringi.

Alternatywnie można użyć biblioteki `OptionParser`, która przekształca argumenty w zrozumiały i użyteczny sposób. Umożliwia to np. rozróżnianie opcji od pozostałych argumentów.

Elixir jest językiem funkcjonalnym, więc argumenty są często przekazywane bezpośrednio do funkcji i przetwarzane w sposób funkcjonalny (jak w przykładzie wykorzystującym Enum.each).

## See Also (Zobacz również)
- [Elixir documentation for System.argv](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir documentation for OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
