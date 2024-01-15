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

## Чому

Якщо ви хочете швидко та ефективно обробляти значну кількість даних, то робота з CSV - саме те, що вам потрібно. CSV (Comma Separated Values) - це формат для збереження табличних даних, який є популярним у різних областях, від бізнесу до науки.

## Як працювати з CSV в Bash

Найпростіший спосіб роботи з CSV в Bash - це використання команди `csvtool`. Нижче наведений приклад коду, який демонструє як створити CSV файл, додати дані до нього та вивести його вміст.

```Bash 
# Створення CSV файлу з двома стовпцями
csvtool col 2 file1.csv file2.csv > data.csv

# Додавання рядків даних в CSV файл
echo "Товар, Ціна" >> data.csv 
echo "Кава, 25" >> data.csv 

# Виведення вмісту CSV файлу
csvtool print data.csv 
«Товар»,«Ціна»
«Кава»,«25»

```

## Глибше вивчення

Якщо ви хочете більше контролю над обробкою CSV файлів в Bash, ви можете використовувати більш складні команди, такі як `grep`, `awk` та `cut`. Також, ви можете використовувати інші утиліти, наприклад `sed` та `tr`, для зміни та обробки даних в CSV форматі.

## Дивіться також

- [Офіційна документація по командам csvtool](https://www.unix.com/man-page/debian/1/csvtool/)
- [Приклади роботи з CSV в Bash](https://www.computerhope.com/unix/usvt.htm)
- [Основні команди Bash](https://www.hostinger.com/tutorials/linux-commands)