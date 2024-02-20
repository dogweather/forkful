---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:30.260283-07:00
description: "Praca z JSON w Lua polega na parsowaniu \u0142a\u0144cuch\xF3w znak\xF3\
  w sformatowanych w JSON do tabel Lua i odwrotnie, co umo\u017Cliwia \u0142atw\u0105\
  \ wymian\u0119 danych mi\u0119dzy\u2026"
lastmod: 2024-02-19 22:04:54.699628
model: gpt-4-0125-preview
summary: "Praca z JSON w Lua polega na parsowaniu \u0142a\u0144cuch\xF3w znak\xF3\
  w sformatowanych w JSON do tabel Lua i odwrotnie, co umo\u017Cliwia \u0142atw\u0105\
  \ wymian\u0119 danych mi\u0119dzy\u2026"
title: Praca z JSON
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z JSON w Lua polega na parsowaniu łańcuchów znaków sformatowanych w JSON do tabel Lua i odwrotnie, co umożliwia łatwą wymianę danych między aplikacjami Lua a usługami sieciowymi lub zewnętrznymi API. Programiści robią to, aby wykorzystać lekki i łatwy do parsowania format JSON dla efektywnego przechowywania danych, konfiguracji lub komunikacji z API.

## Jak to zrobić:
Lua nie zawiera wbudowanej biblioteki do przetwarzania JSON. Dlatego jedną z popularnych bibliotek firm trzecich jest `dkjson`, którą możesz łatwo użyć do kodowania i dekodowania JSON. Najpierw upewnij się, że zainstalowałeś `dkjson`, np. przez LuaRocks (`luarocks install dkjson`), a następnie postępuj zgodnie z poniższymi przykładami.

### Dekodowanie JSON do tabeli Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Programista Lua", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Błąd:", err)
else
  print("Nazwa:", luaTable.name) -- Wyjście: Nazwa: Programista Lua
  print("Wiek:", luaTable.age) -- Wyjście: Wiek: 30
  print("Języki:", table.concat(luaTable.languages, ", ")) -- Wyjście: Języki: Lua, JavaScript
end
```

### Kodowanie tabeli Lua do JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Programista Lua",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Przykładowe wyjście dla kodowania:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Programista Lua"
}
```

Te proste przykłady demonstrują, jak pracować z JSON w Lua, ułatwiając integrację aplikacji Lua z różnymi technologiami internetowymi i zewnętrznymi API. Pamiętaj, że choć w tych przykładach użyto `dkjson`, inne biblioteki takie jak `cjson` i `RapidJSON` mogą również być odpowiednie w zależności od potrzeb twojego projektu.
