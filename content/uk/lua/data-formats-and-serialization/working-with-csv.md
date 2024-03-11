---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:54.715549-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\
  \u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\
  \u043B\u0456\u0437 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\
  \u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0445 \u0434\u0430\u043D\
  \u0438\u0445, \u043E\u0440\u0433\u0430\u043D\u0456\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0445 \u0443 \u0440\u044F\u0434\u043A\u0438 \u0442\u0430 \u0441\u0442\u043E\
  \u0432\u043F\u0446\u0456, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 \u043A\u043E\u043C\u0438\u2026"
lastmod: '2024-03-11T00:14:23.405000-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0440\u043E\
  \u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\u0430\u043C\u0438\
  ) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0430\u043D\u0430\
  \u043B\u0456\u0437 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\
  \u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0445 \u0434\u0430\u043D\
  \u0438\u0445, \u043E\u0440\u0433\u0430\u043D\u0456\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0445 \u0443 \u0440\u044F\u0434\u043A\u0438 \u0442\u0430 \u0441\u0442\u043E\
  \u0432\u043F\u0446\u0456, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u044E\u0447\u0438 \u043A\u043E\u043C\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
---

{{< edit_this_page >}}

## Що та чому?

Робота з файлами CSV (значення, розділені комами) передбачає аналіз та генерацію текстових даних, організованих у рядки та стовпці, використовуючи коми для розділення окремих значень. Програмісти часто займаються цим процесом, щоб полегшити обмін даними між різними програмами, базами даних або для завдань обробки та аналізу даних, завдяки широкій підтримці та простоті CSV.

## Як:

У Lua робота з файлами CSV може бути підходом за допомогою базових операцій введення-виведення файлів, які надає мова, без потреби у зовнішніх бібліотеках для простих завдань. Для більш складних операцій, наприклад, обробки спеціальних випадків (наприклад, коми всередині значень), може бути корисним використання сторонніх бібліотек, як-от `lua-csv`.

### Читання файлу CSV
Ось простий приклад читання файлу CSV рядок за рядком, розділення кожного рядка на значення засноване на роздільнику-комі.

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**Приклад виведення** (для `example.csv` з вмістом "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Запис файлу CSV
Щоб згенерувати файл CSV, ви просто створюєте рядки з кома-розділеними значеннями і записуєте їх у файл рядок за рядком.

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

Цей метод створить (або перезапише) файл `output.csv` з вказаними даними.

### Використання lua-csv
Для більш складного оброблення CSV, включаючи підтримку лапок та символів екранування, бібліотека `lua-csv` є надійним вибором.

Спочатку встановіть її за допомогою LuaRocks:
```shell
luarocks install lua-csv
```

Тоді читання файлу CSV стає таким простим:

```lua
local csv = require("csv")

-- Читання з файлу
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

І запис у CSV з належними лапками та екрануванням:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

Цей підхід автоматично обробляє складності, такі як коми та лапки всередині значень.
