---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:52.524785-07:00
description: "Jak to zrobi\u0107: W Lua nie masz wbudowanej funkcji, kt\xF3ra bezpo\u015B\
  rednio sprawdza, czy katalog istnieje, wi\u0119c cz\u0119sto opierasz si\u0119 na\
  \ bibliotece Lua File\u2026"
lastmod: '2024-03-13T22:44:35.554435-06:00'
model: gpt-4-0125-preview
summary: "W Lua nie masz wbudowanej funkcji, kt\xF3ra bezpo\u015Brednio sprawdza,\
  \ czy katalog istnieje, wi\u0119c cz\u0119sto opierasz si\u0119 na bibliotece Lua\
  \ File System (lfs), popularnej bibliotece stron trzecich do operacji na plikach."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

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
