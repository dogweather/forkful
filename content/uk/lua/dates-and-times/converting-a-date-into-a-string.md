---
date: 2024-01-20 17:37:11.804953-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.519893-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

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
