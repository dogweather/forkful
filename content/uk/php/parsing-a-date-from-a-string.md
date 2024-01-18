---
title:                "Розбір дати з рядка"
html_title:           "PHP: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?

Парсинг дати з рядка - це процес перетворення текстового рядка на дату, яку комп'ютер може розуміти та використовувати. Програмісти виконують цю операцію, щоб перетворити дані, подані у текстовому форматі, в більш зрозумілий для обробки формат.

## Як це зробити:

```PHP
$dateString = "31.12.2020";
$date = date_create_from_format("d.m.Y", $dateString);
echo date_format($date, "l, jS F Y"); //Output: Thursday, 31st December 2020
```

Цей приклад показує використання функції `date_create_from_format` для створення дати з текстового рядка, використовуючи заданий формат дати. Потім ми використовуємо `date_format` для форматування дати у зрозумілий для людини вигляд.

## Глибше занурення:

Історичний контекст:
Парсери дати були введені у PHP 5.2 і вони є частиною стандартної бібліотеки дат PHP.

Інше рішення:
Крім `date_create_from_format`, перетворення дати з рядка також можна виконати за допомогою функції `strtotime`. Однак, її використання може призводити до неправильних результатів, якщо рядок має незрозумілий формат дати.

Деталі реалізації:
Функція `date_create_from_format` приймає два аргументи - формат дати та рядок, який потрібно перетворити. Вона повертає об'єкт дати, який потім можна форматувати за допомогою `date_format`.

## Дивись також:

https://www.php.net/manual/en/function.date-create-from-format.php - Офіційна документація PHP для функції `date_create_from_format`.

https://www.php.net/manual/en/function.strptime.php - Функція `strtotime`.

https://www.php.net/manual/en/datetime.format.php - Офіційна документація для функції `date_format`.