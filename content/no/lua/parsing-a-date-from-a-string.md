---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:37:31.560244-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en tekststreng betyr å omdanne skrevet tekst til en dato et program kan forstå og bruke. Programmerere trenger dette for å håndtere datoer effektivt, for eksempel til å sortere eller lagre hendelser.

## Slik gjør du:
I Lua, bruk standard `os.date` og `os.time` funksjonene:

```Lua
function parseDate(dateString)
    local pattern = "(%d+)-(%d+)-(%d+)"
    local year, month, day = dateString:match(pattern)
    return os.time({year=year, month=month, day=day})
end

local myDateString = "2023-03-15"
local timestamp = parseDate(myDateString)
print(os.date("%Y-%m-%d", timestamp))  -- 2023-03-15
```

Output:
```
2023-03-15
```

## Dykk Ned:
Før Lua, brukte vi C eller Perl for parsing, som ofte var omstendelig. Lua forenkler dette med innebygde funksjoner. Alternativer inkluderer `date` biblioteker som `luadate` for mer komplekse behov. Parsing i seg selv er relativt rett frem, men pass på lokale tidssoner og datoformater.

## Se Også:
- Lua Users Wiki om dato og tid: http://lua-users.org/wiki/DateAndTime
- `luadate` biblioteket for komplekse dato-operasjoner: https://github.com/Tieske/date
