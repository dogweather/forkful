---
date: 2024-01-20 17:55:48.556948-07:00
description: "Czytanie argument\xF3w linii polece\u0144 pozwala na interakcj\u0119\
  \ z twoim programem podczas jego uruchamiania. Programi\u015Bci u\u017Cywaj\u0105\
  \ tego, by uczyni\u0107 aplikacje\u2026"
lastmod: '2024-03-11T00:14:08.231216-06:00'
model: gpt-4-1106-preview
summary: "Czytanie argument\xF3w linii polece\u0144 pozwala na interakcj\u0119 z twoim\
  \ programem podczas jego uruchamiania. Programi\u015Bci u\u017Cywaj\u0105 tego,\
  \ by uczyni\u0107 aplikacje\u2026"
title: "Odczytywanie argument\xF3w linii polece\u0144"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie argumentów linii poleceń pozwala na interakcję z twoim programem podczas jego uruchamiania. Programiści używają tego, by uczynić aplikacje elastycznymi i dostosowywalnymi do potrzeb użytkownika.

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
