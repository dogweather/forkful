---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
W Lua kapitalizacja łańcucha to zmiana pierwszej litery każdego słowa na wielką, a reszty na małe. Programiści używają kapitalizacji, aby ujednolicić teksty (np. w tytułach) i poprawić czytelność.

## How to: (Jak to zrobić:)
```Lua
function capitalizeString(str)
    return (str:gsub("(%a)([%w_']*)", function(first, rest) return first:upper()..rest:lower() end))
end

-- Przykładowe wykorzystanie:

local myString = "witaj w świecie lua!"
local capitalizedString = capitalizeString(myString)
print(capitalizedString) -- "Witaj W Świecie Lua!"
```

## Deep Dive (Głębsze Zanurzenie)
Kapitalizacja łańcucha nie ma długiej historii w Lua, gdyż standardowa biblioteka nie zawiera gotowej funkcji do tego zadania. Musimy napisać własną, jak w powyższym przykładzie. Alternatywnie, niektóre biblioteki zewnętrzne mogą oferować takie narzędzia. Lua operuje na bajtach, więc powyższy kod zakłada użycie systemu kodowania ASCII i może nie działać poprawnie z Unicode bez dodatkowych bibliotek.

## See Also (Zobacz także)
- Dokumentacja Lua: https://www.lua.org/manual/5.4/
- Wprowadzenie do pattern matchingu w Lua: https://www.lua.org/pil/20.2.html
- Repozytorium "Penlight" z zaawansowanymi narzędziami dla Lua: https://github.com/lunarmodules/Penlight

Pamiętaj, że w praktyce kapitalizacja może być bardziej złożona, zwłaszcza przy obsłudze różnych systemów pisania i języków, co może wymagać zastosowania dedykowanych bibliotek do obsługi Unicode, takich jak ICU - International Components for Unicode.
