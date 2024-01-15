---
title:                "Łączenie ciągów znaków"
html_title:           "Elixir: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Większość języków programowania posiada wbudowaną funkcję, służącą do łączenia ciągów znaków, określaną jako "konkatenacja". Pozwala ona na połączenie różnych ciągów znaków w jedną całość, co jest często wykorzystywane przy tworzeniu skryptów i programów. W Elixirze funkcja ta jest nie tylko wygodna, ale również bardzo wydajna, dzięki czemu warto poznać ją bliżej.

## Jak to zrobić?

```elixir
# Przykładowe ciągi znaków
imie = "Anna"
nazwisko = "Nowak"
wiek = 30

# Łączenie za pomocą operatora ++
pelne_imie = imie ++ " " ++ nazwisko

# Łączenie z wykorzystaniem interpolacji
dane_osobowe = "#{imie} #{nazwisko}, wiek #{wiek}"

# Wyświetlenie wyników
IO.puts(pelne_imie) # Wynik: "Anna Nowak"
IO.puts(dane_osobowe) # Wynik: "Anna Nowak, wiek 30"
```

Możliwość łączenia ciągów znaków za pomocą operatora ++ oraz interpolacji pozwala na elastyczne i czytelne tworzenie złożonych ciągów. Warto również zauważyć, że konkatenacja w Elixirze jest niezwykle szybka i nie wpływa na wydajność programu.

## Pogłębiona analiza

W Elixirze konkatenacja jest realizowana przez wykorzystanie list, a więc struktur danych, które dostarczają funkcje do dodawania, usuwania i modyfikowania elementów. W przypadku operatora ++, po lewej stronie musi znajdować się lista, a po prawej dowolna struktura, która może być przekształcona do listy. Dzięki temu dostajemy uniwersalne narzędzie, które możemy wykorzystać w różnych scenariuszach.

## Zobacz również

- [Dokumentacja operatora ++](https://hexdocs.pm/elixir/Kernel.html#++,)
- [Interpolacja w Elixirze](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#string-interpolation)