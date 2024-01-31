---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
CSV, czyli "Comma-Separated Values", to format przechowywania danych w formie tekstowej, gdzie poszczególne wartości oddzielone są przecinkami. Programiści używają CSV ze względu na prostotę i uniwersalność - łatwo jest importować i eksportować dane między różnymi programami i językami programowania.

## Jak to zrobić:
Praca z plikami CSV w Lua jest prosta. Użyjemy standardowego otwierania i czytania plików.

```Lua
-- Otwieranie pliku CSV do odczytu
local plik = io.open('przyklad.csv', 'r')

-- Przechodzenie przez każdą linię pliku
for linia in plik:lines() do
    -- Dzielenie linii na wartości
    local wartosci = {}
    for wartosc in string.gmatch(linia, '([^,]+)') do
        table.insert(wartosci, wartosc)
    end

    -- Tutaj możesz robić coś z wartościami
    for i, val in ipairs(wartosci) do
        print(i, val)
    end
end

-- Nie zapomnij zamknąć pliku
plik:close()
```

Sample output:
```
1 Name
2 Age
3 Country
...
```

## Deep Dive
CSV pochodzi z wczesnych lat 70., gdy organizacje zaczęły używać komputerów do przechowywania dużych ilości danych. Alternatywami dla CSV są formaty jak JSON czy XML, które są bardziej elastyczne, ale również bardziej skomplikowane. Implementując obsługę CSV w Lua, warto zwrócić uwagę na kwestie takie jak obsługa cudzysłowów i nowych linii w wartościach.

## See Also
- [Programming in Lua (Fourth edition)](https://www.lua.org/pil/contents.html)
- [Lua 5.4 reference manual](https://www.lua.org/manual/5.4/)
- [CSV na Wikipedii](https://pl.wikipedia.org/wiki/CSV)
