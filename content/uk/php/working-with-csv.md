---
title:                "Робота з csv"
html_title:           "PHP: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV - це один з способів обробки даних в програмуванні. CSV означає "Comma Separated Values" - це формат збереження даних у вигляді таблиці, де значення розділяються комами. Програмісти використовують CSV для зчитування та запису даних, зокрема для обробки великих обсягів інформації.

## Як?

У PHP є вбудована функція [fgetcsv()](https://www.php.net/manual/en/function.fgetcsv.php), яка дозволяє зчитати дані з CSV файлу у вигляді масиву. Наприклад, якщо у файлі є наступний рядок даних: `John,Smith,25`, то виклик функції `fgetcsv()` поверне масив `['John', 'Smith', '25']`. 



Записувати дані в CSV файл можна за допомогою функції [fputcsv()](https://www.php.net/manual/en/function.fputcsv.php). Наприклад, для запису рядка `['Jane', 'Doe', '30']` у файл потрібно викликати функцію з аргументом файлу і масиву даних: `fputcsv($file, ['Jane', 'Doe', '30'])`.

## Глибше дослідження

Формат CSV був створений для зручності обміну даними між комп'ютерними програмами ще в 1972 році. Хоча зараз є багато альтернативних форматів для зберігання даних, CSV залишається популярним серед програмістів через його простоту та загальну підтримку.

Окрім стандартних функцій PHP, існують також інші бібліотеки, наприклад [league/csv](https://csv.thephpleague.com/), які надають більший функціонал для роботи з CSV файлами. Також можна знайти багато онлайн ресурсів та документації для допомоги з роботою з CSV в PHP.

## Дивіться також

- Документація PHP щодо роботи з CSV: [https://www.php.net/manual/en/ref.filesystem.php](https://www.php.net/manual/en/ref.filesystem.php)
- Офіційна документація формату CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Бібліотека league/csv: [https://csv.thephpleague.com/](https://csv.thephpleague.com/)