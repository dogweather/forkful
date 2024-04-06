---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:07.206353-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.410442-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:


### Чтение из CSV файла:
```Bash
while IFS=, read -r col1 col2 col3
do
  echo "Колонка 1: $col1 | Колонка 2: $col2 | Колонка 3: $col3"
done < myfile.csv
```

Пример вывода:

```
Колонка 1: data1 | Колонка 2: data2 | Колонка 3: data3
```

### Запись в CSV файл:
```Bash
echo "data1,data2,data3" > myfile.csv
```

### Добавление в CSV файл:
```Bash
echo "data4,data5,data6" >> myfile.csv
```

## Подробнее
Формат CSV имеет корни в раннем компьютинге и стал основным в обмене данными, потому что он поддерживается широким спектром программного обеспечения. Хотя Bash может работать с файлами CSV, он не предназначен для сложного разбора. Альтернативы для более сложных задач включают AWK, Sed или использование полноценного языка программирования, например, Python. Детали реализации, которые следует учитывать при работе с CSV в Bash, включают обработку специальных символов, сложные кавычки и переносы строк в полях.

## Смотрите также
- [Документация GNU Coreutils](https://www.gnu.org/software/coreutils/)
- [Руководство по Bash](https://www.gnu.org/software/bash/manual/)
- [Введение в AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Sed на Примерах](https://www.gnu.org/software/sed/manual/sed.html)

Для более продвинутой манипуляции с CSV:
- [Документация по модулю CSV в Python](https://docs.python.org/3/library/csv.html)
- [Библиотека Pandas для Python](https://pandas.pydata.org/)
