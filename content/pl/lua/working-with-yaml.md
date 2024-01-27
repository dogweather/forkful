---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
YAML to łatwy w użyciu format służący do reprezentacji danych, często używany w konfiguracji aplikacji. Programiści wybierają go dla jego czytelności i prostoty.

## Jak to zrobić:
Aby pracować z YAML w Lua, musisz użyć zewnętrznej biblioteki, np. lyaml. Poniżej znajdziesz przykład użycia:

```Lua
local lyaml = require('lyaml')

-- Załaduj YAML z ciągu znaków
local yaml_str = [[
- just: plain
- YAML: rocks
- Lua: is cool too
]]

-- Parsowanie YAML do Lua table
local data = lyaml.load(yaml_str)

-- Wyświetlanie przetworzonych danych
for index, value in ipairs(data) do
  for key, val in pairs(value) do
    print(key, val)
  end
end
```

Wyjście:
```
just	plain
YAML	rocks
Lua	is cool too
```

## Dogłębnie:
YAML, czyli "YAML Ain't Markup Language", powstał w 2001 roku jako język łatwy do czytania przez ludzi. Alternatywami są JSON czy XML, ale YAML jest często preferowany ze względu na swoją prostotę w konfiguracjach aplikacji. Integracja z Lua odbywa się poprzez biblioteki takie jak `lyaml`, które implementują funkcje do przetwarzania formatu YAML.

## Zobacz również:
- Oficjalna strona YAML: https://yaml.org
- Repozytorium `lyaml` na GitHub: https://github.com/gvvaughan/lyaml
- YAML vs JSON: https://json2yaml.com/what-is-yaml/json-vs-yaml
