---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:56.135801-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0412\
  \ \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u044B\u0445 \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\u0445 Lua \u043D\u0435\u0442 \u0432\
  \u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u043E\u0431\u0440\u0430\u0431\
  \u043E\u0442\u043A\u0438 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0438\u0439\
  . \u0427\u0430\u0441\u0442\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044E\u0442 `os.execute` \u0441 `test` \u043D\u0430 Unix \u0438\u043B\u0438 `os.getenv`\
  \ \u043D\u0430 Windows.\u2026"
lastmod: '2024-03-13T22:44:45.315207-06:00'
model: gpt-4-0125-preview
summary: "\u0412 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u044B\u0445\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\u0445 Lua \u043D\u0435\
  \u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u043E\u0431\
  \u0440\u0430\u0431\u043E\u0442\u043A\u0438 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\
  \u0440\u0438\u0439."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

## Как сделать:
В стандартных библиотеках Lua нет встроенной обработки директорий. Часто используют `os.execute` с `test` на Unix или `os.getenv` на Windows. Вот как это делается:

```lua
local function is_directory_exists(path)
    if package.config:sub(1,1) == '\\' then -- проверка для Windows
        local cd_result = os.execute('cd ' .. path .. ' 2>nul')
        return cd_result == true or cd_result == 0
    else -- предполагаем систему похожую на Unix
        local test_result = os.execute('[ -d "' .. path .. '" ]')
        return test_result == true or test_result == 0
    end
end

print(is_directory_exists("/path/to/check/")) -- Системы похожие на Unix
print(is_directory_exists("C:\\path\\to\\check\\")) -- Системы Windows
```

Пример вывода может быть просто `true`, если директория существует, или `false`, если её нет.

## Подробнее
В ранние годы развития вычислительной техники управление файлами было критически важно в операционных системах, и проверка существования директорий выполнялась простыми командами оболочки. Lua, хотя и разработана для встраивания и расширения, остается минималистичной и, таким образом, полагается на внешние вызовы для таких задач.

Функция `os.execute` в Lua выполняет системную команду, делая её универсальной для этой цели. Системы на базе Unix хорошо реагируют на флаг `-d`, который проверяет наличие директорий. В Windows попытка смены директории с помощью `cd` служит нашей проверке.

Есть альтернативы, например, библиотека `lfs` (LuaFileSystem), которая предоставляет `lfs.attributes(path, "mode")`, более надежный и читаемый метод для выполнения той же задачи, но это требует установки дополнительных зависимостей.

По причинам производительности, прямые системные вызовы могут быть быстрее, чем использование полной библиотеки, особенно для простых задач, таких как проверка существования директории. Однако, использование `os.execute` имеет накладные расходы из-за создания нового процесса, поэтому будьте осторожны в плотном цикле.

## Смотрите также
- Документация LuaFileSystem: http://keplerproject.github.io/luafilesystem/manual.html
- Справочник по библиотеке `os` Lua: https://www.lua.org/manual/5.4/manual.html#6.9
- "Программирование на Lua" для более глубокого понимания языка: https://www.lua.org/pil/
