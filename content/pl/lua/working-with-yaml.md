---
title:                "Praca z yaml"
html_title:           "Lua: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z YAML to nic innego jak pracowanie z plikami tekstowymi. Programiści używają YAML do przechowywania i przesyłania danych w sposób czytelny dla człowieka. Jest to popularna metoda w branży IT ze względu na łatwą edycję i spójność z językiem programowania Lua.

## Jak to zrobić:

```Lua
local yaml = require("yaml")
local data = yaml.load([[
name: John
age: 28
hobby:
- reading
- painting
]])

print(data.name, data.age)
for _, item in ipairs(data.hobby) do
    print(item)
end
```
Output:
```
John 28
reading
painting
```

## Wgląd w głąb:

YAML został stworzony w 2001 roku i jest często wykorzystywany wraz z językami skryptowymi, takimi jak Lua. Alternatywami dla YAML są JSON i XML, jednak YAML jest uważany za prostszy i czytelniejszy dla człowieka.

Aby pracować z YAML w Lua, należy zainstalować odpowiedni moduł za pomocą menadżera pakietów, na przykład LuaRocks. W przypadku manualnej instalacji, konieczne będzie ręczne dodanie ścieżki do modułu w konfiguracji Lua.

## Zobacz także:

- Oficjalna dokumentacja YAML: https://yaml.org/spec/
- Biblioteka LuaYAML: https://github.com/gvvaughan/luayaml