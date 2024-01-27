---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON to format wymiany danych, lekki i łatwo czytelny dla ludzi. Programiści używają JSON do komunikacji między serwerem a klientem oraz do przechowywania danych.

## How to:

Do obsługi JSON w Lua użyjemy biblioteki `dkjson`. Najpierw zainstaluj ją przez luarocks lub pobierz bezpośrednio.

```Lua
-- Zainstaluj dkjson poprzez luarocks
luarocks install dkjson
```

Kodowanie i dekodowanie:

```Lua
local json = require "dkjson"

-- Kodowanie do JSON
local data = { name = "Marek", age = 30, likesLua = true }
local json_data = json.encode(data)
print(json_data)  -- Output: {"name":"Marek","age":30,"likesLua":true}

-- Dekodowanie z JSON
local decoded_data = json.decode(json_data)
print(decoded_data.name)  -- Output: Marek
```

## Deep Dive

JSON, czyli JavaScript Object Notation, powstał jako podzbiór języka JavaScript. Dzisiaj jest standardem niezależnym od platformy. Alternatywą dla JSON jest np. XML, lecz JSON jest lżejszy i szybszy. W Lua JSON nie jest wbudowany, więc korzysta się z bibliotek zewnętrznych, takich jak `dkjson`, `cjson`, czy `lua-cjson`.

## See Also

- Dokumentacja `dkjson`: http://dkolf.de/src/dkjson-lua.fsl/home
- Specyfikacja JSON: https://www.json.org/json-pl.html
- LuaRocks `dkjson`: https://luarocks.org/modules/dkolf/dkjson
- Json.lua, alternatywna biblioteka: https://github.com/rxi/json.lua
