---
date: 2024-01-20 17:45:41.867122-07:00
description: "Wyci\u0105ganie podci\u0105g\xF3w (ang. \"substring extraction\") to\
  \ spos\xF3b na wyj\u0119cie specyficznej cz\u0119\u015Bci z ci\u0105gu znak\xF3\
  w. Programi\u015Bci to robi\u0105, \u017Ceby pracowa\u0107 tylko z\u2026"
lastmod: '2024-03-13T22:44:35.029818-06:00'
model: gpt-4-1106-preview
summary: "Wyci\u0105ganie podci\u0105g\xF3w (ang."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## Jak to zrobić:
W Elixirze, używamy głównie funkcji `String.slice/3`, żeby wyciągnąć podciąg.

```elixir
# Wyciąganie podciągu zakresu znaków
string = "Hello Elixir World"
substring = String.slice(string, 6, 6) # Wyciągnięcie słowa "Elixir"
IO.puts(substring)

# Gdy zna się początkowy i końcowy indeks
start_index = 6
end_index = 11
IO.puts(String.slice(string, start_index..end_index)) # Tak samo, "Elixir"

# Użycie negatywnych indeksów (liczone od końca)
IO.puts(String.slice(string, -5, 5)) # Wydobędzie "World"
```

Output:
```
Elixir
Elixir
World
```

## Głębsze spojrzenie:
Historia Elixir to historia poszukiwania jak najlepszego rozwiązania w świecie Erlanga. Do wersji 1.0 w Elixirze dodane zostały wyrafinowane narzędzia do pracy ze stringami, w tym wyciąganie podciągów. Alternatywą jest użycie regexu (`Regex` moduł), ale to dobrze mieć dedykowaną, wydajną funkcję. 

Pod względem implementacji `String.slice/3` jest wystarczająco inteligentna, by radzić sobie z różnymi rodzajami ciągów różnych długości oraz uwzględnia Unicode (co jest nie-trivialne!). Pamiętajmy, w Elixirze indeksowanie jest oparte na bajtach, nie znakach, co jest ważne przy pracy z Unicode.

## Zobacz także:
1. [Oficjalna dokumentacja modułu String w Elixirze](https://hexdocs.pm/elixir/String.html)
2. [Obsługa Unicode w Elixirze](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
3. [Dokumentacja modułu Regex w Elixirze](https://hexdocs.pm/elixir/Regex.html)
