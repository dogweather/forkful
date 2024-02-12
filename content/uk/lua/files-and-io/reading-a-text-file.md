---
title:                "Читання текстового файлу"
aliases:
- /uk/lua/reading-a-text-file.md
date:                  2024-01-20T17:54:43.396189-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Читання текстового файлу - це процес здобуття даних з файлу, який зберігає текст. Програмісти роблять це, щоб працювати з даними, налаштовувати програми чи аналізувати інформацію.

## How to / Як це робити:
```Lua
-- Відкриття файла для читання
local file = io.open("example.txt", "r")

-- Читання всього файлу
local content = file:read("*a")
print(content)

-- Закриваєм файл
file:close()
```
Якщо у файлі 'example.txt' буде текст "Привіт, світ!", то вивід буде:
```
Привіт, світ!
```

## Deep Dive / Поглиблений Розділ:
Читання файлів у Lua має давню історію, але принципи залишились незмінними із часів Lua 5.1. Альтернативи, як luaposix бібліотека, існують для Unix-подібних систем. Під час читання файлу, Lua використовує буферизацію, щоб ефективно працювати з диском. Функціонал 'io' бібліотеки дозволяє читати файли по рядках, частинами, або цілком, використовуючи `file:read()` з різними аргументами.

## See Also / Дивіться Також:
- [Programming in Lua (official book)](https://www.lua.org/pil/)
- [Lua 5.4 Reference Manual – I/O library](https://www.lua.org/manual/5.4/manual.html#6.8)
