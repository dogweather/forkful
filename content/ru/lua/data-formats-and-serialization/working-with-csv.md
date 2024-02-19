---
aliases:
- /ru/lua/working-with-csv/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:47.697886-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u0438\u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0435\u043D\u043D\
  \u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\u0438) \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\
  \u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445, \u0440\u0430\u0437\u0434\
  \u0435\u043B\u0435\u043D\u043D\u044B\u0445 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\
  \u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0438\u0437-\u0437\u0430\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0442\u044B \u0438\u2026"
lastmod: 2024-02-18 23:08:57.176998
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV (\u0437\u043D\u0430\u0447\
  \u0435\u043D\u0438\u044F, \u0440\u0430\u0437\u0434\u0435\u043B\u0435\u043D\u043D\
  \u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\u0438) \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\
  \u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445, \u0440\u0430\u0437\u0434\
  \u0435\u043B\u0435\u043D\u043D\u044B\u0445 \u0437\u0430\u043F\u044F\u0442\u044B\u043C\
  \u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\
  \ \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E \u0438\u0437-\u0437\u0430\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0442\u044B \u0438\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
---

{{< edit_this_page >}}

## Что и Почему?

Работа с CSV (значения, разделенные запятыми) означает анализ и генерацию текстовых данных, разделенных запятыми. Программисты делают это из-за простоты и взаимодействия - почти каждая система и язык поддерживают CSV, что делает его очевидным выбором для обмена данными.

## Как:

Давайте читать и записывать файлы CSV с помощью Lua. Мы рассмотрим базовый пример без внешних библиотек.

**Чтение файла CSV:**

```Lua
function read_csv(filepath)
  local results = {}
  local file = assert(io.open(filepath, "r"))
  
  for line in file:lines() do
    table.insert(results, line:split(","))
  end
  
  file:close()
  return results
end

-- Вспомогательная функция для деления строк
function string:split(delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = self:find(delimiter, from, true)
  while delim_from do
    table.insert(result, self:sub(from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = self:find(delimiter, from, true)
  end
  table.insert(result, self:sub(from))
  return result
end
```

**Запись в файл CSV:**

```Lua
function write_csv(filepath, data)
  local file = assert(io.open(filepath, "w"))
  
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  
  file:close()
end

-- Пример данных
local data = {
  { "Имя", "Возраст", "Город" },
  { "Алиса", "30", "Нью-Йорк" },
  { "Боб", "25", "Лос-Анджелес" }
}

write_csv("output.csv", data)
```

## Углубляемся

История CSV уходит корнями в ранние дни вычислительной техники, когда ключевым было простота. Хотя теперь JSON и XML предлагают более богатые структуры данных, CSV остается популярным из-за своей читаемости и простоты редактирования с помощью табличного программного обеспечения. Когда дело доходит до реализации, следует быть внимательными к полям с запятыми, переносами строк или кавычками - они должны быть должным образом заключены в кавычки и/или экранированы.

## Смотрите также

- Официальное руководство по Lua 5.4: https://www.lua.org/manual/5.4/
- RFC 4180, Общий формат и MIME-тип для файлов с значен
- Penlight Lua Libraries (для более продвинутой работы с CSV): https://github.com/lunarmodules/Penlight
