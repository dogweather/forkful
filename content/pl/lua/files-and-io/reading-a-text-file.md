---
date: 2024-01-20 17:54:48.058550-07:00
description: "Jak to zrobi\u0107: Je\u015Bli plik \"przykladowy_tekst.txt\" zawiera:\
  \ \"Witaj, \u015Bwiecie!\", wynikiem b\u0119dzie."
lastmod: '2024-04-05T21:53:36.985556-06:00'
model: gpt-4-1106-preview
summary: "Je\u015Bli plik \"przykladowy_tekst.txt\" zawiera."
title: Odczytywanie pliku tekstowego
weight: 22
---

## Jak to zrobić:
```Lua
-- Otworzenie pliku do odczytu:
local file = io.open("przykladowy_tekst.txt", "r")

-- Sprawdzenie, czy plik został poprawnie otwarty:
if not file then
    error("Nie można otworzyć pliku.")
else
    -- Czytanie całej zawartości pliku:
    local zawartosc = file:read("*a")
    print(zawartosc)
    
    -- Zamykanie pliku:
    file:close()
end
```

Jeśli plik "przykladowy_tekst.txt" zawiera: "Witaj, świecie!", wynikiem będzie:
```
Witaj, świecie!
```

## Deep Dive
Czytanie plików w Lua, języku powstałym w Brazylii w 1993 roku, jest podobne do innych języków - otwiera się plik, czyta zawartość, a potem zamyka. Istnieją alternatywne metody, takie jak używanie `io.lines` do czytania pliku linia po linii – świetne dla dużych plików. Elementy takie jak "file handles" i metody odczytu (`*a` dla całej zawartości, `*l` dla jednej linii, `*n` dla liczby) dają elastyczność w manipulacji danymi.

## Zobacz również
- [Oficjalny manual Lua](https://www.lua.org/manual/5.4/)
- [Dyskusje na Stack Overflow](https://stackoverflow.com/questions/tagged/lua+file-io)
