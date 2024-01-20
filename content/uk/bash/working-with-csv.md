---
title:                "Робота з csv"
html_title:           "Bash: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?
Обробка CSV є необхідною частиною роботи багатьох програмістів. CSV - це формат файлу, який дозволяє зберігати дані у вигляді таблиць, зрозумілих для людини. Це дозволяє програмістам зберігати, обробляти та аналізувати великі обсяги даних.

## Як:
```Bash
# Для читання CSV файлу:
while IFS=',' read -r column1 column2 column3
do
    # код для обробки даних
    echo "Рядок: $column1, $column2, $column3"
done < file.csv

# Для створення нового CSV файлу:
echo "Name, Age, Gender" >> new_file.csv
echo "John, 25, Male" >> new_file.csv
echo "Jane, 30, Female" >> new_file.csv
```
Вивід:
```
Рядок: Значення першої колонки, Значення другої колонки, Значення третьої колонки
Рядок: Ім'я, Вік, Стать
Рядок: John, 25, Male
Рядок: Jane, 30, Female
```

## Deep Dive:
1. Історичний контекст: CSV був створений у 1972 році та був одним з перших форматів для зберігання та обробки даних у вигляді таблиць.
2. Альтернативи: Існують інші формати файлів, які також можуть зберігати дані у вигляді таблиць, наприклад XML та JSON. Однак, CSV є більш простим та зрозумілим для людини форматом.
3. Деталі реалізації: Для роботи з CSV у Bash використовується команда "read", яка дозволяє читати та обробляти рядки файлу за роздільниками, у нашому випадку - це кома.