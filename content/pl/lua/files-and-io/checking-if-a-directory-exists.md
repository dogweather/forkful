---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- /pl/lua/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:52.524785-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, to podstawowa operacja podczas pisania skryptów wchodzących w interakcję z systemem plików, zapewniająca, że program działa na prawidłowych ścieżkach i zapobiegająca błędom związanym z nieistniejącymi katalogami. Zadanie to jest kluczowe przy tworzeniu nowych plików w katalogach, czytaniu z nich lub bezpiecznym wykonywaniu operacji związanych z katalogami.

## Jak to zrobić:

W Lua nie masz wbudowanej funkcji, która bezpośrednio sprawdza, czy katalog istnieje, więc często opierasz się na bibliotece Lua File System (lfs), popularnej bibliotece stron trzecich do operacji na plikach.

Najpierw upewnij się, że masz zainstalowany Lua File System. Jeśli nie, możesz go zazwyczaj zainstalować za pomocą LuaRocks:

```sh
luarocks install luafilesystem
```

Następnie możesz użyć poniższego przykładu, aby sprawdzić istnienie katalogu:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- Sprawdź, czy konkretny katalog istnieje
if directoryExists("/path/to/your/directory") then
    print("Katalog istnieje.")
else
    print("Katalog nie istnieje.")
end
```

To wyświetli:

```
Katalog istnieje.
```

Albo, jeśli katalog nie istnieje:

```
Katalog nie istnieje.
```

To podejście wykorzystuje funkcję `lfs.attributes`, aby uzyskać atrybuty ścieżki. Jeśli ścieżka istnieje i jej atrybut `mode` to `directory`, potwierdza to istnienie katalogu.
