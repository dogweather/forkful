---
title:                "Znalezienie długości ciągu znaków"
aliases: - /pl/elixir/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:13.534224-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Znalezienie długości łańcucha znaków oznacza ustalenie, ile znaków zawiera dany tekst. Programiści robią to do walidacji, formatowania, oraz by kontrolować przepływ danych.

## Jak to zrobić:
Elixir używa funkcji `String.length/1` do znalezienia długości stringa. Oto jak to działa:

```Elixir
# Użyj String.length/1, by znaleźć długość stringa
string = "Witajcie, programiści!"
długość = String.length(string)
IO.puts(długość)
```

Wynik działania:
```
22
```

## Dogłębna analiza
W Elixirze, stringi są binarnymi i reprezentują Unicode jako UTF-8. W historii języków programowania różnie to wyglądało; na przykład w C używano `strlen`, by znaleźć długość stringa. Alternatywą w Elixirze jest `byte_size/1`, która zwraca liczbę bajtów w stringu, co nie zawsze równa się liczbie znaków ze względu na UTF-8.

```Elixir
# byte_size zamiast String.length
string = "ń"
IO.puts(String.length(string))
IO.puts(byte_size(string))
```

Wynik:
```
1
2
```

Pokażemy więc '1' dla długości, ale '2' dla rozmiaru bajtowego, gdyż 'ń' zajmuje więcej niż jeden bajt.

## Zobacz także
- Dokumentacja Elixir: https://hexdocs.pm/elixir/String.html
- Tutorial UTF-8 w Elixirze: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
