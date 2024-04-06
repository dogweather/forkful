---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:23.744006-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: 1. \u0427\u0442\u0435\u043D\u0438\u0435 \u0444\u0430\u0439\u043B\u0430\
  \ CSV \u043F\u043E\u0441\u0442\u0440\u043E\u0447\u043D\u043E."
lastmod: '2024-04-05T22:38:45.017179-06:00'
model: gpt-4-0125-preview
summary: "1. \u0427\u0442\u0435\u043D\u0438\u0435 \u0444\u0430\u0439\u043B\u0430 CSV\
  \ \u043F\u043E\u0441\u0442\u0440\u043E\u0447\u043D\u043E."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:
1. Чтение файла CSV построчно:
```Fish Shell
for line in (cat file.csv)
    echo $line
end
```

2. Разделение полей и вывод определённого столбца (например, второго столбца):
```Fish Shell
cat file.csv | while read -l line
    set -l fields (string split "," $line)
    echo $fields[2]
end
```

3. Запись в файл CSV:
```Fish Shell
echo "name,age,city" > users.csv
echo "Alice,30,New York" >> users.csv
echo "Bob,25,Los Angeles" >> users.csv
```

Пример вывода (содержимое `users.csv`):
```
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

## Подробнее
Обработка CSV существует с первых дней персональных компьютеров, развиваясь как простой формат для обмена данными. Несмотря на свою базовость, отсутствие стандарта CSV может привести к проблемам при анализе, например, к различиям в разделителях и кодировке текста. Хотя в Fish Shell нет встроенных инструментов для разбора CSV, часто используются `awk`, `sed` и `cut` вместе с ним для выполнения более сложных задач.

Подход Fish к CSV более ручной и основан на скриптах, используя его возможности манипуляций со строками для обработки полей CSV. Для обработки больших объемов данных рассмотрите альтернативы, такие как библиотека `pandas` в Python или инструменты командной строки, такие как `csvkit`.

## Смотрите также
- Начало работы с `awk`: [AWK - Учебник и Введение](https://www.grymoire.com/Unix/Awk.html)
- Введение в `sed`: [Sed - Введение и Учебник](https://www.grymoire.com/Unix/Sed.html)
- Официальная документация Fish Shell: [Документация Fish Shell](https://fishshell.com/docs/current/index.html)
- Документация `csvkit`: [csvkit - Набор утилит для работы с CSV](https://csvkit.readthedocs.io/en/latest/)
