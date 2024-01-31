---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego to proces tworzenia nowego pliku lub modyfikacji istniejącego, aby zawierał określony tekst. Programiści to robią, gdy chcą zachować dane, skonfigurować oprogramowanie lub przechować wyniki działania programu.

## Jak to zrobić:
```Lua
-- Otwieranie pliku do zapisu
local plik = io.open("przyklad.txt", "w")

-- Sprawdź, czy plik został pomyślnie otwarty
if plik then
    -- Zapisz tekst do pliku
    plik:write("Witaj, świecie!\nTo jest kolejna linia tekstu.")
    -- Zamknij plik
    plik:close()
else
    print("Nie można otworzyć pliku do zapisu.")
end
```

Output w pliku `przyklad.txt`:
```
Witaj, świecie!
To jest kolejna linia tekstu.
```

## W głębi tematu:
W przeszłości do zapisywania tekstów używano bardziej prymitywnych metod, np. punch cards. W Lua, oprócz standardowego `io.open`, możliwe jest także wykorzystanie innych bibliotek, takich jak `lfs` (LuaFileSystem) dla zaawansowanych operacji na plikach. Implementacja zapisu wykonuje się przez posłużenie się buforowanym wyjściem, co zwiększa wydajność poprzez minimalizację operacji I/O.

## Zobacz również:
- Oficjalny tutorial Lua do obsługi wejścia/wyjścia: https://www.lua.org/pil/21.2.html
- Dokumentacja LuaFileSystem (lfs): https://keplerproject.github.io/luafilesystem/manual.html
