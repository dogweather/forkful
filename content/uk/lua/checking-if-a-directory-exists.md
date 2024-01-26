---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:57:50.685945-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що це & Чому?

Перевірка наявності директорії важлива для запобігання помилок файла та управління потоком програми. Програмісти роблять це, щоб безпечно читати, писати, чи маніпулювати файлами в директорії.

## Як це зробити:

```Lua
local lfs = require("lfs") -- завантажуємо LuaFileSystem бібліотеку

local function directoryExists(directory)
    return lfs.attributes(directory, "mode") == "directory"
end

-- Перевірка директорії
local directoryPath = "/path/to/directory"
if directoryExists(directoryPath) then
    print("Директорія існує!")
else
    print("Директорії не знайдено.")
end
```

Приклад виводу, якщо директорія існує:
```
Директорія існує!
```

## Поглиблений аналіз:

Перевірка існування директорії - це не завжди було так легко. У ранніх версіях Lua, такі функціональності не існувало і потребувало обхідних шляхів, таких як виклик системних команд. LuaFileSystem (LFS) - це бібліотека, яка заповнила цей пробіл, надаючи портативний спосіб роботи з файловою системою. Альтернативи включають користування сторонніми модулями або вбудованими функціями ОС через `io` та `os` пакети, але вони можуть бути нестабільні та залежні від платформи. LuaFileSystem забезпечує чистий, переносимий інтерфейс до файлової системи.

## Дивись також:

- [LuaFileSystem офіційний репозиторій GitHub](https://github.com/keplerproject/luafilesystem)
- [Документація Lua 5.4](https://www.lua.org/manual/5.4/)
- [Lua користувацький вікі](http://lua-users.org/wiki/) - Чудове місце для вчитися більше про Lua та обміну знаннями з іншими розробниками.
