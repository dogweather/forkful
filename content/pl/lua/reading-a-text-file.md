---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:54:48.058550-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/reading-a-text-file.md"
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
