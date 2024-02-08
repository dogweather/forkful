---
title:                "Работа с CSV"
aliases:
- ru/elixir/working-with-csv.md
date:                  2024-01-29T00:03:40.658115-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

CSV (Comma-Separated Values, значения, разделенные запятыми) - это формат простого текста для табличных данных. Программисты используют CSV для легкого обмена большими наборами данных между различными программами, сервисами или базами данных, где сложность не требуется.

## Как это сделать:

Elixir изначально не включает в себя парсинг CSV в свою стандартную библиотеку, но вы можете использовать hex-пакет `CSV`. Вот быстрый пример, чтобы начать:

```elixir
# Сначала добавьте `{:csv, "~> 2.4"}` в файл mix.exs и выполните `mix deps.get`
# Затем используйте модуль CSV следующим образом:

CSV.decode("name,age\nJohn Doe,27\nJane Smith,32", headers: true)
|> Enum.map(fn(row) -> 
  "Привет, #{row["name"]}, который имеет #{row["age"]} лет!"
end)
```

Пример выходных данных:

```
["Привет, John Doe, который имеет 27 лет!", "Привет, Jane Smith, который имеет 32 года!"]
```

## Подробнее

CSV не новинка; он существует с начала 1970-х годов, что делает его одним из самых долговечных форматов файлов. Его простота является его главным достоинством и недостатком. Среди альтернатив — JSON, XML или бинарные форматы, такие как Protocol Buffers, каждый из которых имеет свои компромиссы в сложности, размере и читаемости. Что касается Elixir, когда вы декодируете данные CSV с использованием пакета `CSV`, под капотом он бесшовно обрабатывает общие проблемы, такие как преобразование типов данных, экранирование и кодировку символов.

## См. также

- Документация hex-пакета `CSV`: <https://hexdocs.pm/csv>
- Введение в модуль Stream в Elixir для больших CSV: <https://elixir-lang.org/getting-started/enumerables-and-streams.html>
- Сравнение форматов файлов (CSV, JSON, XML и др.): <https://www.ibm.com/docs/en/iis/11.5?topic=formats-comparing-file-reactivity>
