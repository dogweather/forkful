---
date: 2024-01-26 04:25:05.427429-07:00
description: "Jak to zrobi\u0107: Po pierwsze, upewnij si\u0119, \u017Ce Twoje \u015B\
  rodowisko Lua ma parser TOML. U\u017Cyjemy w tym przyk\u0142adzie `lua-toml`."
lastmod: '2024-03-13T22:44:35.563568-06:00'
model: gpt-4-0125-preview
summary: "Po pierwsze, upewnij si\u0119, \u017Ce Twoje \u015Brodowisko Lua ma parser\
  \ TOML."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Po pierwsze, upewnij się, że Twoje środowisko Lua ma parser TOML. Użyjemy w tym przykładzie `lua-toml`.

```Lua
local toml = require("toml")

-- Analiza łańcucha TOML
local toml_data = [[
title = "Przykład TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "Przykład TOML"

-- Generowanie łańcucha TOML
local table_data = {
  title = "Przykład TOML",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Przykładowe wyniki:
```
Przykład TOML
```

## Pogłębiona analiza
TOML został stworzony przez Toma Preston-Wernera w 2013 roku jako alternatywa dla innych języków serializacji danych, takich jak XML i YAML, oferując prostszy format do reprezentowania danych konfiguracyjnych. Chociaż JSON jest wszechobecny, jego składnia może być uciążliwa dla plików konfiguracyjnych. TOML wyróżnia się czytelniejszą składnią dla ludzi, przypominającą pliki .ini, ale z możliwościami zagnieżdżania i typami danych.

Alternatywy dla TOML to JSON, YAML i XML. Jednak TOML jest specjalnie zaprojektowany do konfiguracji i można argumentować, że jest prostszy niż YAML, bardziej czytelny niż JSON do celów konfiguracyjnych i mniej rozwlekły niż XML.

Implementacja obsługi TOML w Lua zazwyczaj wymaga biblioteki stron trzecich. Wydajność i funkcje mogą się różnić, od podstawowej analizy po pełne wsparcie serializacji. Podczas pracy z dużymi plikami konfiguracyjnymi lub częstymi operacjami odczytu/zapisu należy rozważyć wydajność biblioteki i zgodność z najnowszą wersją TOML.

## Zobacz także
- Specyfikacja TOML: https://toml.io/en/
- Biblioteka `lua-toml`: https://github.com/jonstoler/lua-toml
- Porównanie formatów serializacji danych: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
