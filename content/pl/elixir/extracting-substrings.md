---
title:                "Elixir: Ekstrakcja podciągów"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyciąganie podciągów jest jedną z najczęściej używanych funkcji w programowaniu w Elixirze. Pozwala nam na wybieranie części ciągu znaków ze zmiennej lub arytmetycznej i używanie ich w różnych celach. W tym przewodniku omówimy, dlaczego jest to ważne i jak to zrobić w Elixirze.

## Jak to zrobić

Poniżej przedstawiamy przykłady kodu, które pokazują jak wyciągać podciągi w Elixirze. Kod jest zawarty w blokach ```Elixir ... ``` dla łatwej lektury. Przetestuj każdy przykład w swoim środowisku, aby zobaczyć, jak działa w praktyce.

### Wyciąganie podciągu ze zmiennej

```Elixir
str = "Witaj świecie!"
IO.puts(String.slice(str, 0, 5))
```

Ten przykład pokazuje jak używać funkcji `String.slice` do wyciągania podciągu ze zmiennej `str`. Wynik powyższego kodu będzie wyglądał następująco:

```
Witaj
```

### Wyciąganie podciągu za pomocą indeksów

```Elixir
str = "Elixir jest super!"
IO.puts(String.slice(str, 7..12))
```

Ten przykład pokazuje, jak możesz użyć zakresów indeksów, aby wyciągnąć określony podciąg ze zmiennej `str`. Wynik tego kodu będzie wyglądał tak:

```
jest s
```

### Wyciąganie podciągu z wykorzystaniem wyrażenia regularnego

```Elixir
str = "Jestem super programistą w Elixirze!"
IO.puts(str |> String.split(" ") |> Enum.find(&match?(&1, ~r/.+programistą/)))
```

Ten przykład pokazuje, jak użyć wyrażenia regularnego do wyszukania podciągu w zmiennej `str`. Wynik powyższego kodu będzie wyglądał następująco:

```
super programistą
```

## Deep Dive

Wyciąganie podciągów jest możliwe dzięki funkcji `String.slice` oraz wyrażeniom regularnym. Funkcja `String.slice` przyjmuje dwa argumenty: zmienną, z której chcemy wyciągnąć podciąg, oraz indeks początkowy i końcowy, który określa, które znaki w zmiennej będą uwzględnione w podciągu. Wyrażenia regularne, takie jak `~r/.+programistą/`, są wyrażeniami, które pozwalają na bardziej precyzyjne określenie, jakiego podciągu szukamy w zmiennej.

## Zobacz również

- [Dokumentacja Elixir - String.slice](https://hexdocs.pm/elixir/String.html#slice/3)
- [Wyrażenia regularne w Elixirze](https://elixirschool.com/pl/lessons/advanced/pattern-matching/#-regular-expressions)
- [Przewodnik po Elixirze na NaszKod.pl](https://naszkod.pl/2017-03/Elixir-przewodnik-jak-dziala-jak-naprawic)