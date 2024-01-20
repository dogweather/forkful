---
title:                "Praca z plikami csv"
html_title:           "Lua: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

W programowaniu, często spotykasz się z plikami CSV, zwane także plikami tekstowymi przechowującymi tabularne dane. CSV oznacza "Comma Separated Values", co oznacza, że dane są oddzielone przecinkami. Programiści często pracują z CSV, ponieważ jest to łatwy sposób na przechowywanie i dostęp do danych w formie tabelarycznej.

## Jak to zrobić:

```Lua
local csv = require("csv")

-- Ładowanie danych z pliku CSV
local data = csv.read("przykladowy_plik.csv")

-- Dostęp do danych (wiersze i kolumny używając indeksów)
local pierwszy_wiersz = data[1]
local pierwsza_kolumna = data[1][1]

-- Pętla przez wszystkie wiersze i wypisywanie wartości kolumn
for i, wiersz in ipairs(data) do
  print("Wiersz " .. i)
  for j, kolumna in ipairs(wiersz) do
    print("Kolumna " .. j .. ": " .. kolumna)
  end
end

-- Zapisywanie danych do nowego pliku CSV
csv.write("nowy_plik.csv", data)
```

Przykładowy plik CSV:

| Imię  | Nazwisko | Wiek |
|-------|----------|------|
| Jan   | Kowalski | 30   |
| Anna  | Nowak    | 25   |
| Marek | Szymański | 35  |

Wypisane wartości:

```
Wiersz 1
Kolumna 1: Jan
Kolumna 2: Kowalski
Kolumna 3: 30

Wiersz 2
Kolumna 1: Anna
Kolumna 2: Nowak
Kolumna 3: 25

Wiersz 3
Kolumna 1: Marek
Kolumna 2: Szymański
Kolumna 3: 35
```

## Głębszy Zanurzenie:

Pierwsza wersja pliku CSV została stworzona w roku 1972 i od tego czasu jest szeroko używana jako format przechowywania danych. Jedną z alternatyw dla pracy z CSV jest używanie baz danych, ale CSV jest często preferowane do przechowywania prostej tabelarycznej struktury danych. Implementacja modułu CSV dla języka Lua jest napisana w czystym Lua i nie wymaga dodatkowych zależności.

## Zobacz też:

- [Podstawowe operacje na plikach z formatem CSV w języku Lua](https://medium.com/@luapermutation/basic-csv-file-operations-in-lua-8f079e1fa520)