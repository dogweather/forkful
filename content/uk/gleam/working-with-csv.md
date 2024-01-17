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

Що & Чому?

Робота з CSV - це процес обробки та маніпулювання даними у форматi Comma-Separated Values. Програмiсти зазвичай працюють з CSV, оскільки цей формат є загальноприйнятим для обміну даними між різними програмами та системами.

Як?

Нижче наводяться приклади коду та вихідних даних, які допоможуть вам розібратися, як працювати з CSV в Gleam.

```Gleam
import csv

my_file = csv.read("my_data.csv")
row_count = my_file |> List.length
```

Ви можете отримати доступ до окремих рядків чи колонок даних, використовуючи функції утиліти CSV, наприклад `row["column_name"]` або `get_column(file, "column_name")`.

Глибше занурення

CSV (Comma-Separated Values) вперше з'явився в 1972 році, коли компанія IBM використовувала його для обміну даними між електронними таблицями. Це стало популярним форматом для переміщення та обробки даних, оскільки він досить простий та універсальний.

Існують інші формати для зберігання даних, які можуть бути більш потужними та гнучкими, такі як JSON або XML. Однак, використання CSV залишається популярним завдяки його простоті та широкій підтримці серед програм.

Приведений приклад використання `csv.read`, що зображений в розділі "Як?", є лише одним із багатьох способів роботи з CSV в Gleam. Ви можете також записувати дані у CSV файл та використовувати функції для додавання рядків та стовпців.

Дивіться також

Тут перелічені деякі корисні посилання для отримання більш детальної інформації про роботу з CSV у Gleam:

- Офіційна документація Gleam з розділом про CSV: https://gleam.run/documentation/stdlib/csv
- Детальний огляд роботи з CSV у Gleam: https://medium.com/@gleam_lang/csv-handling-in-gleam-10bd1ded4c00
- Приклади коду для роботи з CSV у Gleam: https://github.com/search?q=language%3Agleam+csv&type=code