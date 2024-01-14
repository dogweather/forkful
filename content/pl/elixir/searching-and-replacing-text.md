---
title:                "Elixir: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli jesteś programistą lub programistką Elixir, na pewno zetknąłeś się z sytuacją, w której musiałeś/musiałaś zmienić pewien fragment tekstu w swoim kodzie. To bardzo częsta potrzeba w świecie programowania, ponieważ kod ulega zmianom i często potrzebujemy dostosować go do nowych wymagań. Dzięki prostym narzędziom dostępnym w Elixir, te zmiany mogą być szybkie i bezbolesne, co ułatwia nam pracę i pozwala oszczędzić wiele czasu. W tym artykule dowiesz się, jak w prosty sposób wyszukiwać i zmieniać tekst w kodzie Elixir.

## Jak to zrobić?

W Elixir mamy do dyspozycji funkcję `String.replace/3`, która umożliwia nam przeszukiwanie tekstu i jego zmianę. Składnia funkcji wygląda następująco:

```Elixir
String.replace(text, pattern, replacement)
```

Gdzie `text` to tekst, w którym chcemy przeprowadzić zmiany, `pattern` to wzorzec, który chcemy znaleźć, a `replacement` to tekst, który chcemy wstawić w miejsce znalezionego wzorca.

Przykładowo, jeśli mamy następujący tekst:

```Elixir
"Kocham Elixir"
```

i chcemy zmienić słowo "Elixir" na "programowanie", możemy użyć funkcji `String.replace/3` w ten sposób:

```Elixir
String.replace("Kocham Elixir", "Elixir", "programowanie")
```

W efekcie otrzymamy tekst:

```Elixir
"Kocham programowanie"
```

Możemy również użyć funkcji wewnątrz innej funkcji, na przykład w pozycji argumentu, co pozwala nam wprowadzać zmiany bezpośrednio w naszym kodzie. Przykładowo, jeśli mamy listę z nazwami funkcji, możemy użyć `Enum.map/2` wraz z `String.replace/3` do zmiany nazwy na pisane małymi literami:

```Elixir
["Funkcja1", "Funkcja2", "Funkcja3"] 
|> Enum.map(&String.replace(&1, &1, String.downcase(&1)))
```

W wyniku otrzymamy listę z elementami:

```Elixir
["funkcja1", "funkcja2", "funkcja3"]
```

## Wielokrotne zmiany

Czasami może się zdarzyć, że potrzebujemy przeprowadzić wiele zmian w jednym tekście. W takiej sytuacji przydatna będzie funkcja `String.replace/4`, która pozwala na przeprowadzenie wielu zmian za pomocą listy par klucz-wartość.

```Elixir
String.replace("Java jest dobra", [{"Java", "Elixir"}, {"dobra", "niesamowita"}])
```

W efekcie otrzymamy tekst:

```Elixir
"Elixir jest niesamowita"
```

## Deep Dive

W Elixir mamy również dostęp do bardziej zaawansowanych funkcji, takich jak `Regex.replace/3`, która pozwala nam na przeszukiwanie tekstu za pomocą wyrażeń regularnych. W przypadku dużych i skomplikowanych zmian, korzystanie z wyrażeń regularnych może być preferowane, ponieważ daje nam większą kontrolę nad tym, co chcemy znaleźć i zmienić.

## Zobacz także

- Dokumentacja: https://hexdocs.pm/elixir/String.html#replace/3
- Poradnik o wyrażeniach regularnych w Elixir: https://dev.to/mttrs/regex-in-elixir-3gdi