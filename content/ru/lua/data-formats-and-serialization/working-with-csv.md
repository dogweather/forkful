---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:47.697886-07:00
description: "\u041A\u0430\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0447\
  \u0438\u0442\u0430\u0442\u044C \u0438 \u0437\u0430\u043F\u0438\u0441\u044B\u0432\
  \u0430\u0442\u044C \u0444\u0430\u0439\u043B\u044B CSV \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E Lua. \u041C\u044B \u0440\u0430\u0441\u0441\u043C\u043E\u0442\u0440\
  \u0438\u043C \u0431\u0430\u0437\u043E\u0432\u044B\u0439 \u043F\u0440\u0438\u043C\
  \u0435\u0440 \u0431\u0435\u0437 \u0432\u043D\u0435\u0448\u043D\u0438\u0445 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A. **\u0427\u0442\u0435\u043D\u0438\
  \u0435 \u0444\u0430\u0439\u043B\u0430 CSV:**."
lastmod: '2024-03-13T22:44:45.329411-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0447\u0438\u0442\u0430\u0442\
  \u044C \u0438 \u0437\u0430\u043F\u0438\u0441\u044B\u0432\u0430\u0442\u044C \u0444\
  \u0430\u0439\u043B\u044B CSV \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E Lua."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
