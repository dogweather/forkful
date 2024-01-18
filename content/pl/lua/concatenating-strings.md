---
title:                "Łączenie ciągów znaków."
html_title:           "Lua: Łączenie ciągów znaków."
simple_title:         "Łączenie ciągów znaków."
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Łączenie ciągów to podstawowa operacja w programowaniu, polegająca na połączeniu dwóch lub więcej ciągów znakowych w jeden ciąg. Programiści często używają jej do tworzenia bardziej czytelnych i złożonych wyrażeń lub do budowania dynamicznych komunikatów dla użytkowników.

## Jak to zrobić?

```lua
-- Przykład 1:  Użycie operatora '..' do łączenia ciągów
local imie = "Jan"
local nazwisko = "Kowalski"
local pelneNazwisko = imie .. " " .. nazwisko -- wynik: "Jan Kowalski"

-- Przykład 2: Użycie funkcji table.concat() do łączenia elementów z tabeli
local lista = {"jabłko", "banan", "pomarańcza"}
local listaOwocow = table.concat(lista, ", ") -- wynik: "jabłko, banan, pomarańcza"
```

## Głębsze zagadnienia

Łączenie ciągów było od dawna wykorzystywane w różnych językach programowania i jest uważane za podstawową umiejętność dla programistów. W niektórych językach, jak na przykład Python, istnieje wbudowana funkcja `join()`, która ułatwia łączenie elementów z tabeli.

W Lua, oprócz operatora `..` i funkcji `table.concat()`, istnieje także metoda `string.concat()`, jednak może ona być użyta tylko na dwóch ciągach równocześnie.

## Zobacz także

- Dokumentacja Lua o łączeniu ciągów: https://www.lua.org/manual/5.4/manual.html#6.4.1
- Wideo-tutorial o łączeniu ciągów w Lua: https://www.youtube.com/watch?v=U6UpcUDWThU
- Porównanie różnych sposobów łączenia ciągów w różnych językach programowania: https://blog.codinghorror.com/new-programming-jargon/