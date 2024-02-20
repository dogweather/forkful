---
date: 2024-01-20 17:54:48.058550-07:00
description: "Czytanie pliku tekstowego to proces wyci\u0105gania danych z pliku znajduj\u0105\
  cego si\u0119 na dysku. Programi\u015Bci robi\u0105 to, \u017Ceby obs\u0142u\u017C\
  y\u0107 zawarto\u015B\u0107 \u2013 wy\u015Bwietli\u0107 j\u0105,\u2026"
lastmod: 2024-02-19 22:04:54.695396
model: gpt-4-1106-preview
summary: "Czytanie pliku tekstowego to proces wyci\u0105gania danych z pliku znajduj\u0105\
  cego si\u0119 na dysku. Programi\u015Bci robi\u0105 to, \u017Ceby obs\u0142u\u017C\
  y\u0107 zawarto\u015B\u0107 \u2013 wy\u015Bwietli\u0107 j\u0105,\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## Co & Dlaczego?
Czytanie pliku tekstowego to proces wyciągania danych z pliku znajdującego się na dysku. Programiści robią to, żeby obsłużyć zawartość – wyświetlić ją, zmodyfikować, albo przetworzyć w jakiś użyteczny sposób.

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
