---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:51.692891-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: **\u0427\u0442\u0435\u043D\u0438\u0435 CSV:**."
lastmod: '2024-03-13T22:44:46.037820-06:00'
model: gpt-4-0125-preview
summary: "**\u0427\u0442\u0435\u043D\u0438\u0435 CSV:**."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
