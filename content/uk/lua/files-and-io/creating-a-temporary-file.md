---
date: 2024-01-20 17:40:45.164739-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.532862-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

## How to: (Як це зробити:)
```Lua
local os = require("os")

-- Створення унікального тимчасового файлу
local temp_filename = os.tmpname()

-- Використання тимчасового файлу
local temp_file = io.open(temp_filename, "w")
temp_file:write("Це просто приклад.")
temp_file:close()

-- Перевірка його створення
print("Тимчасовий файл створений: " .. temp_filename)

-- Видалення тимчасового файлу
os.remove(temp_filename)
print("Тимчасовий файл видалений.")
```
Sample output:
```
Тимчасовий файл створений: /tmp/lua_aBc123
Тимчасовий файл видалений.
```

## Deep Dive (Поглиблений Розбір):
Тимчасові файли не новина, вони використовуються ЩЕ з часів Unix. В Lua, `os.tmpname()` створює назву для тимчасового файлу без створення самого файлу, а `io.open()` уже відкриває та створює файл. Інші мови мають свої інструменти, але в Lua все лаконічно і просто. Головне пам'ятати про видалення тимчасового файлу після використання, адже Lua не робить це самостійно.

## See Also (Дивись Також):
- Lua `io` library: https://www.lua.org/manual/5.4/manual.html#6.8
- Lua `os` library: https://www.lua.org/manual/5.4/manual.html#6.9
- Робота з файлами в Lua: https://lua-users.org/wiki/IoLibraryTutorial
