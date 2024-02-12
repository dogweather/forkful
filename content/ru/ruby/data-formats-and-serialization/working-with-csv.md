---
title:                "Работа с CSV"
aliases: - /ru/ruby/working-with-csv.md
date:                  2024-01-29T00:03:51.692891-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
CSV (файлы с разделителями-запятыми) представляют табличные данные в виде простого текста. Программисты используют CSV для импорта и экспорта наборов данных, потому что они широко поддерживаются, легко читаются и просты в разборе.

## Как это сделать:

**Чтение CSV:**

```Ruby
require 'csv'

CSV.foreach("path/to/file.csv", headers: true) do |row|
  puts row["HeaderName"] # Замените на ваше настоящее название столбца
end
```

**Запись в CSV:**

```Ruby
require 'csv'

CSV.open("path/to/output.csv", "wb", write_headers: true, headers: ["Name", "Age", "City"]) do |csv|
  csv << ["Alice", 32, "Wonderland"]
  csv << ["Bob", 46, "Springfield"]
end
```

**Пример вывода:**

```Text
Alice, 32, Wonderland
Bob, 46, Springfield
```

## Подробнее

CSV существуют с ранних дней компьютерной эры, предлагая простой способ перемещения табличных данных между программами и системами. Альтернативы включают JSON и XML, но CSV остаются популярными из-за их простоты и низких затрат на обработку. Стандартная библиотека CSV Ruby, удобно обернутая вокруг основных парсеров, предлагает беспрепятственную интеграцию, включая поддержку разных кодировок, пользовательские конвертеры и гибкие варианты разбора.

## Смотрите также

- Документация библиотеки CSV Ruby: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- CSV в Википедии: https://en.wikipedia.org/wiki/Comma-separated_values
- Gem "FasterCSV" (старый, но актуален для исторических причин): https://rubygems.org/gems/fastercsv
