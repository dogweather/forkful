---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Перетворення дати у рядок в PHP: Що це таке і навіщо це потрібно?

## Що і чому?
Перетворення дати у рядок - це процес зміни формату дати з об'єкта або масива до рядка. Програмісти роблять це, щоб масштабувати і зберігати дату у зручній і зрозумілої формі.

## Як це робити:
PHP надає вбудовану функцію `date()`, що змінює формат Unix timestamp в текстове представлення дати і часу. Ось як використовувати цю функцію:
```PHP
<?php
$date = time(); // Отримуємо поточний Unix timestamp
$date_string = date("Y-m-d H:i:s", $date); // Перетворюємо дату в рядок
echo $date_string; // Виводимо рядок
?>
```
Ви побачите вивід у форматі `YYYY-MM-DD HH:MM:SS`, наприклад `2022-01-01 12:00:00`.

## Поглиблене вивчення:
У минулому, при відсутності потужних бібліотек для роботи з датами, програмісти активно використовували формат Unix timestamp. Однак, оснащена різними варіантами форматування дати, функція PHP `date()` взяла на себе важливу роль. 

Альтернативі може бути клас `DateTime` з методом `format()`, який також дає можливістьПеретворення дати у рядок. Ось приклад використання:
```PHP
<?php
$date = new DateTime(); // Отримуємо поточну дату і час
$date_string = $date->format("Y-m-d H:i:s"); // Перетворюємо дату в рядок
echo $date_string; // Виводимо рядок
?>
```
Окрім функції форматування дати, метод `DateTime::format()` ще і узгоджує часові зони.

## Додатково:
- [PHP date() Function](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime::format](https://www.php.net/manual/en/datetime.format.php)
- [UNIX Timestamp, Datetime, Conversion in PHP](https://www.geekhideout.com/php-unix-time.php)