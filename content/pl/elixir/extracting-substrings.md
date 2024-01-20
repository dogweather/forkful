---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Ekstrakcja podciągów to operacja odseparowania określonej części ciągu znaków. Programiści to robią, aby uzyskać określone fragmenty danych lub manipulować tylko określonymi segmentami ciągu.

## Jak to zrobić:
Możemy używać wbudowanej funkcji `String.slice/2` w Elixir do ekstrakcji podciągow. Oto kilka przykładów: 

```elixir 
string = "Witaj, Elixir!"

# Pobierz podciąg od 3 znaku do końca 
IO.puts String.slice(string, 3) 
# -> "taj, Elixir!"

# Pobierz podciąg od 2 do 5 znaku
IO.puts String.slice(string, 2, 3)
# -> "taj"

# Użyj ujemnego indeksu, aby zacząć od końca 
IO.puts String.slice(string, -7, 6) 
# -> "Elixir"
```

## Głębsze spojrzenie
Technika ekstrakcji podciągów istnieje od dawna, w wielu językach programowania. W późniejszych wersjach Elixir, funkcja `slice/2` jest zaimplementowana przy użyciu algorytmu charakteryzującego się efektywnością przez powtarzalne odczyty.

Alternatywnie, możemy używać `binary_part/3`, choć jest zdecydowanie mniej czytelna dla nas. Warto wiedzieć, że `String.slice/2` i `binary_part/3` są identyczne pod względem wydajności.

```elixir 
IO.puts :erlang.binary_part("Witaj, Elixir!", 2, 3) 
# -> "taj"
```

## Zobacz także
Sprawdź [dokumentację Elixir String](https://hexdocs.pm/elixir/String.html) i [Erlang :binary module](http://erlang.org/doc/man/binary.html) do bardziej szczegółowego omówienia funkcji i możliwości manipulacji ciągami.