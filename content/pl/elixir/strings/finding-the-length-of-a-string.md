---
date: 2024-01-20 17:47:13.534224-07:00
description: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w oznacza\
  \ ustalenie, ile znak\xF3w zawiera dany tekst. Programi\u015Bci robi\u0105 to do\
  \ walidacji, formatowania, oraz by\u2026"
lastmod: '2024-03-13T22:44:35.032468-06:00'
model: gpt-4-1106-preview
summary: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha znak\xF3w oznacza ustalenie,\
  \ ile znak\xF3w zawiera dany tekst."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
