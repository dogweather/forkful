---
title:                "Работа с CSV"
aliases: - /ru/lua/working-with-csv.md
date:                  2024-01-29T00:04:47.697886-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/lua/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
