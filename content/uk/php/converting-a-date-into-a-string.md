---
title:                "PHP: Перетворення дати у рядок"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Робота з датами є важливою частиною веб-програмування. Конвертування дати в рядок допомагає нам працювати з датами в більш зручному форматі, що є важливим для роботи з датами в PHP.

## Як це зробити

```PHP
<?php
$date = date("d/m/Y", strtotime("2021-03-25"));
echo $date;
```

Виведе: 25/03/2021

```PHP
<?php
$date = date("l, F d, Y", strtotime("2021-03-25"));
echo $date;
```

Виведе: Четвер, Березень 25, 2021

## Глибоке дослідження

Формат виведення дати можна налаштувати за допомогою функції `date()`. У функції можна вказати будь-який формат дати, за яким буде здійснюватися конвертація. Крім того, PHP має ряд вбудованих функцій для роботи з датами, таких як `strtotime()` або `date_diff()`, які дозволяють здійснювати розрахунки з датами і зручно виводити їх результати в рядок.

## Дивіться також

- [Робота з датами в PHP](https://www.php.net/manual/uk/datetime.php)
- [Функція date()](https://www.php.net/manual/uk/function.date.php)
- [Функція strtotime()](https://www.php.net/manual/uk/function.strtotime.php)