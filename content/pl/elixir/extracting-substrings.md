---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:45:41.867122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyciąganie podciągów (ang. "substring extraction") to sposób na wyjęcie specyficznej części z ciągu znaków. Programiści to robią, żeby pracować tylko z tym, co akurat potrzebują – może to być sprawdzanie formatu, pobieranie danych, czy jakakolwiek manipulacja tekstem.

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
