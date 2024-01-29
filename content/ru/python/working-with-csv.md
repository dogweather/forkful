---
title:                "Работа с CSV"
date:                  2024-01-29T00:04:14.311495-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Работа с файлами CSV (Comma-Separated Values, значения, разделённые запятыми) означает чтение из и запись данных в простые текстовые файлы, где каждая строка является записью данных. Программисты ценят CSV файлы за их лёгкость, читаемость для человека и совместимость практически с любым инструментом обработки данных.

## Как это делать:
```python
# Импорт модуля CSV
import csv

# Чтение файла CSV
with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Вывод:
# ['Name', 'Age', 'City']
# ['Alice', '30', 'New York']
# ...

# Запись в файл CSV
with open('output.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Name', 'Age', 'City'])
    writer.writerow(['Bob', '22', 'Los Angeles'])

# Проверьте output.csv, чтобы увидеть результаты
```

## Погружение
В те времена, когда передача данных была медленнее и хранение данных дороже, CSV завоевал поклонников за его простоту и низкие накладные расходы. Альтернативы типа JSON и XML предоставляют структуру, но за счёт развернутости. Для CSV скорость парсинга является преимуществом, однако могут возникнуть сложности с обработкой сложных иерархий или типов данных.

Библиотеки типа `pandas` также могут работать с CSV, предлагая большую мощность, но требуя больших ресурсов. Внутри csv.reader() является генератором, выдающим строки одну за другой — умно с точки зрения управления памятью.

## Смотрите также
- Документация Python по чтению/записи CSV: https://docs.python.org/3/library/csv.html
- Библиотека `pandas` для обработки сложных данных: https://pandas.pydata.org/
- CSV против JSON против XML: Сравнение форматов данных: https://www.datacamp.com/community/tutorials/json-xml-csv
