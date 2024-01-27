---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Bash: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Обробка CSV файлів – це робота з таблицями даних у текстовому форматі, де значення розділені комами. Програмісти працюють з CSV, бо це простий спосіб зберігати та обмінюватися табличними даними.

## Як це зробити:
```Bash
# Читання CSV файлу та виведення стовпців
awk -F, '{ print $1 ", " $2 }' data.csv

# Вибір ліній, які відповідають певному критерію
grep 'Kyiv' data.csv

# Додавання нового рядка у CSV файл
echo '4, Kyiv, 2800000' >> data.csv
```
## Поглиблено:
CSV, або Comma-Separated Values, – формат файлу, започаткований у 1970-х. Альтернативи: JSON, XML. При роботі з CSV часто використовують утиліти командного рядка, скрипти на Bash або мови програмування як python чи ruby, що мають вбудовані бібліотеки для роботи з CSV.

## Дивись також:
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - стандарт по роботі з CSV.
- [Bash scripting cheatsheet](https://devhints.io/bash) - шпаргалка по Bash скриптам.
