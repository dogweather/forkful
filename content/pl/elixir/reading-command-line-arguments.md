---
title:    "Elixir: Odczytywanie argumentów wiersza poleceń"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinieneś interesować się czytaniem argumentów wiersza poleceń? Jeśli piszesz w języku Elixir, umiejętność czytania argumentów z wiersza poleceń jest niezbędna do tworzenia aplikacji wiersza poleceń lub do programów, które wymagają podawania parametrów przy uruchamianiu. W tym artykule dowiesz się, jak w łatwy sposób czytać argumenty wiersza poleceń w Elixir.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie modułu `OptionParser`, który jest częścią standardowej biblioteki Elixir. Następnie możesz zdefiniować listę opcji, które chcesz obsługiwać przy uruchamianiu aplikacji. Na przykład, jeśli chcesz móc przekazać opcję `-f` w celu wyświetlenia pliku, możesz to zrobić w następujący sposób:

```Elixir
OptionParser.parse(argv, switches: [force: :boolean])
```

Argument `argv` zawiera listę argumentów wiersza poleceń przekazanych do aplikacji. Następnie możesz użyć funkcji `force` zwracanej przez `OptionParser.parse` w celu sprawdzenia, czy opcja `-f` została podana:

```Elixir
if force do
  IO.puts("Wyświetlam plik...")
end
```

Innym przykładem jest przekazywanie wartości liczbowych jako argumentów. Pozwala to na dynamiczne generowanie danych przy uruchamianiu aplikacji. Możesz to zrobić poprzez określenie parametru jako typu `:positive_integer` w liście opcji:

```Elixir
OptionParser.parse(argv, switches: [count: :positive_integer])
```

A następnie wyświetlić liczbę określoną jako `count`:

```Elixir
IO.puts("Generuję #{count} losowych liczb...")
```

## Deep Dive

Podczas czytania argumentów z wiersza poleceń można również określić domyślne wartości dla opcji lub przekazać funkcję do wywołania, jeśli opcja zostanie podana. Możesz również użyć `OptionParser.on_unknown_option` do obsługi nieznanych opcji. Jest to przydatne, gdy chcesz umożliwić użytkownikom podawanie własnych opcji, ale chcesz mieć kontrolę nad obsługa nieznanych przypadków.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o modułach lub innych funkcjonalnościach języka Elixir, przeczytaj więcej na poniższych stronach:

- [Dokumentacja języka Elixir](https://elixir-lang.org/docs.html)
- [Moduł OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Artykuł o działaniach wiersza poleceń w Elixir](http://erlangenbies.github.io/post/cli-in-elixir/)