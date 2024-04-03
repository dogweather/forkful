---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:14.311495-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: ."
lastmod: '2024-03-13T22:44:44.312838-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
