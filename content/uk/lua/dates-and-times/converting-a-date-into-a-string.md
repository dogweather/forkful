---
title:                "Перетворення дати в рядок"
aliases:
- /uk/lua/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:11.804953-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Перетворення дати в рядок — це процес форматування дати та часу для читабельності чи зберігання. Програмісти роблять це, щоб легше вивести дату на екран, або зберегти у вигляді, зручному для бази даних чи файлів.

## How to: (Як це зробити:)
```Lua
-- Завантажуємо бібліотеку 'os'
local os = require("os")

-- Отримуємо поточну дату і час
local current_time = os.time()

-- Конвертуємо в рядковий формат
local date_string = os.date("%Y-%m-%d %H:%M:%S", current_time)

print(date_string)  -- Виводимо рядок дати
```
Sample output:
```
2023-04-01 12:45:23
```

## Deep Dive (Поглиблено:)
Дата і час в програмуванні завжди важливі для логування, таймінгу подій та взаємодії з користувачами. Lua використовує функції з бібліотеки C (через `os.date` та `os.time`) для работы з часом. Цей підхід, що спирається на бібліотеку операційної системи, дає переносимість коду. Є альтернативи стандартній бібліотеці, як-от luadate, що пропонують розширені можливості. При перетворенні дати в рядок важливо обрати правильний формат; Lua дозволяє це зробити за допомогою рядка формату.

## See Also (Див. також):
- [Lua 5.4 Reference Manual - os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Lua 5.4 Reference Manual - os.time](https://www.lua.org/manual/5.4/manual.html#pdf-os.time)
- [GitHub - luadate: Date and Time library for Lua](https://github.com/Tieske/date)
