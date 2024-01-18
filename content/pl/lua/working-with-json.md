---
title:                "Praca z json"
html_title:           "Lua: Praca z json"
simple_title:         "Praca z json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z JSON to popularna czynność w świecie programowania. JSON jest formatem danych zapisanych w postaci tekstu, często wykorzystywanym do przesyłania informacji między serwerami a aplikacjami internetowymi. Programiści często pracują z JSON, ponieważ jest on prosty w użyciu i czytelny dla ludzi, co ułatwia komunikację z innymi programistami i integrację z różnymi systemami.

## Jak to zrobić?

W Lua, do pracy z JSON używamy modułu ```cjson```. Przykładowe użycie można zobaczyć poniżej:

```Lua
-- Importujemy moduł cjson
local cjson = require("cjson")

-- Przykładowy JSON
local json_data = '{"name": "Jan", "age": 25, "languages": ["Lua", "Python"]}'

-- Dekodowanie JSON do tabeli
local decoded_data = cjson.decode(json_data)

-- Wyświetlenie wartości z tabeli
print("Imię: " .. decoded_data.name)
print("Wiek: " .. decoded_data.age)
print("Znane języki: " .. table.concat(decoded_data.languages, ", "))
```

Wynik:

```
Imię: Jan
Wiek: 25
Znane języki: Lua, Python
```

## W głębi

JSON został stworzony w 2001 roku i stał się popularny w codziennym użytkowaniu ze względu na swoją prostotę. Alternatywami dla JSON są między innymi format XML oraz format CSV. Moduł cjson jest częścią standardowej biblioteki Lua, co oznacza, że nie trzeba go pobierać i instalować osobno.

## Zobacz też

Dokumentacja modułu cjson: https://www.lua.org/manual/5.3/manual.html#6.4

W porównaniu do innych języków programowania, Lua ma ograniczone wsparcie dla pracy z JSON. Jeśli szukasz bardziej rozbudowanej obsługi JSON w Lua, warto rozważyć używanie dodatkowych bibliotek lub przetłumaczenie z Lua na język, który ma wbudowane wsparcie dla JSON.