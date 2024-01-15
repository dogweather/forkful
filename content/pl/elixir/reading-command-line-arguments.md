---
title:                "Wczytywanie argumentów z linii poleceń"
html_title:           "Elixir: Wczytywanie argumentów z linii poleceń"
simple_title:         "Wczytywanie argumentów z linii poleceń"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy są w stanie odczytywać argumenty z linii poleceń? W tym artykule dowiesz się, dlaczego jest to ważne i jak w praktyce wykorzystać czytanie argumentów z linii poleceń w Elixir.

## Jak to zrobić

Aby odczytać argumenty z linii poleceń w Elixir, należy użyć funkcji `System.argv/0` lub `System.argv/1`. Pierwsza z nich zwraca listę argumentów jako atomy, a druga jako ciągi znaków. Aby zobaczyć to w praktyce, przejdźmy do kodu:

```elixir
# Przykładowy kod wykorzystujący `System.argv/0`
args = System.argv()

IO.puts "Otrzymane argumenty: #{inspect args}"
```

```elixir
# Przykładowy kod wykorzystujący `System.argv/1`
args = System.argv("--flag argument1 argument2")

IO.puts "Otrzymany wynik: #{inspect args}"
```

Po uruchomieniu powyższych przykładowych kodów, powinniśmy otrzymać odpowiednio takie wyniki:

`Otrzymane argumenty: [:elixir, :arg1, :arg2]`

`Otrzymany wynik: ["--flag", "argument1", "argument2"]`

Jak widać, funkcje `System.argv/0` i `System.argv/1` zwracają odpowiednio listę atomów i listę stringów. Dzięki temu możemy wygodnie odczytywać i przetwarzać podane argumenty.

## Głębokie zanurzenie

Pozwólmy sobie na nieco głębsze zanurzenie w temat czytania argumentów z linii poleceń w Elixir. Warto wiedzieć, że funkcje `System.argv/0` i `System.argv/1` nie są jedynymi sposobami odczytywania argumentów. Możemy również skorzystać z modułu `OptionParser`, który oferuje dodatkowe funkcjonalności, takie jak parsowanie flag czy argumentów wykorzystujących wartości domyślne.

Poniższy przykład pokazuje, jak możemy korzystać z `OptionParser` w celu odczytania argumentów z linii poleceń:

```elixir
# Przykład użycia `OptionParser`
args = ["--flag", "argument1", "argument2"]

options = OptionParser.parse(args, [parse_flags: true, strict: false])

IO.inspect options
```

W powyższym przykładzie, `OptionParser` zostanie użyty do parsowania argumentów `args`, uwzględniając ustawione flagi. Wynikiem będzie mapa zawierająca wszystkie parsowane opcje i wartości.

## Zobacz także

- [Dokumentacja Elixir na temat czytania argumentów z linii poleceń](https://hexdocs.pm/elixir/System.html#argv/0)
- [Więcej informacji na temat modułu `OptionParser`](https://hexdocs.pm/elixir/OptionParser.html)
- [Przykładowe kody, w których wykorzystywane są odczytywanie argumentów z linii poleceń w Elixir](https://gist.github.com/search?utf8=✓&q=elixir+command+line+args&type=)