---
title:                "Робота з CSV файлами"
date:                  2024-01-19
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
CSV (Comma-Separated Values) — формат файла, у якому дані розділені комами. Програмісти використовують CSV для легкого обміну даними між різними системами та програмами.

## Як це зробити:
Простий скрипт на Lua для читання CSV:

```Lua
function parseCsvLine(line, sep)
    local res = {}
    local searchPattern = string.format('([^%s]+)', sep)

    for str in string.gmatch(line, searchPattern) do
        table.insert(res, str)
    end

    return res
end

local file = io.open('example.csv', 'r')
for line in file:lines() do
    local fields = parseCsvLine(line, ',')
    -- Обробка полів
    print(table.concat(fields, '|'))
end
file:close()
```

Припустимо, у `example.csv` є такі дані:
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Результат:
```
name|age|city
Alice|30|New York
Bob|25|Los Angeles
```

## Поглиблено:
CSV виник у 1970-х для обміну даними. Серед альтернатив — JSON та XML. Перевага CSV: простота та широка підтримка. Однак, CSV має обмеження, наприклад, відсутність типізації даних.

## Дивись також:
- Lua Users Wiki: http://lua-users.org/wiki
- RFC 4180, стандарт CSV: https://tools.ietf.org/html/rfc4180
- Лекції про обробку CSV у Lua: https://www.tutorialspoint.com/lua/lua_file_io.htm
