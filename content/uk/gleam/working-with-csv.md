---
title:                "Робота з csv"
html_title:           "Gleam: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Робота з CSV - це потужний інструмент для обробки та аналізу даних у форматі таблиці. Для українських розробників, які працюють з даними, це надзвичайно важливий навичка, оскільки вони можуть експортувати, імпортувати та маніпулювати даними з CSV-файлів без особливих труднощів.

## Як зробити

Основним інструментом для роботи з CSV у Gleam є модуль [csv](https://gleam.run/modules/csv.html), який містить функції для читання та запису даних з CSV-файлів. Наприклад, для читання даних з вхідного файлу і створення нового CSV-файлу з цими даними, ми можемо використати такий код:

```Gleam
import csv

let input_file = "input.csv"
let output_file = "output.csv"

// Читаємо дані з вхідного файлу
let records = csv.read(input_file)

// Записуємо дані в новий файл
csv.write(output_file, records)
```

Цей код буде прочитати дані з CSV-файлу з назвою "input.csv" та створить новий CSV-файл з назвою "output.csv", який буде містити ті самі дані. Також, модуль [csv](https://gleam.run/modules/csv.html) містить інші функції для маніпулювання та обробки даних з CSV-файлів, які можна вивчити в офіційній документації Gleam.

## Глибоке занурення

Для більш глибокого занурення в роботу з CSV у Gleam, рекомендуємо ознайомитися з [CSV репозиторієм](https://github.com/gleam-lang/csv) на GitHub, де можна знайти приклади коду, документацію та інші корисні ресурси. Також, офіційний форум Gleam є чудовим місцем для обговорення та отримання допомоги в разі виникнення проблем з роботою з CSV.

## Дивіться також

- [Офіційна документація Gleam](https://gleam.run/documentation/)
- [Центр допомоги Gleam](https://help.gleam.run/)
- [Gleam CSV репозиторій на GitHub](https://github.com/gleam-lang/csv)