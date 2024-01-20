---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:44.072922-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?

Парсинг дати - це процес конвертації текстової строки у дату, яку розуміє PHP. Програмісти роблять це, щоб можна було легко зберігати, порівнювати та обробляти дати.

## Як це зробити:

```php
<?php
$dateString = "2023-04-01 14:00:00";
$dateObject = new DateTime($dateString);
echo $dateObject->format('Y-m-d H:i:s'); // Виведення: 2023-04-01 14:00:00

// Якщо формат дати невідомий або складний
$dateStringComplex = "April 1st, 2023, 2:00 PM";
$dateObjectComplex = DateTime::createFromFormat('F jS, Y, g:i A', $dateStringComplex);
echo $dateObjectComplex->format('Y-m-d H:i:s'); // Виведення: 2023-04-01 14:00:00
?>
```

## Поглиблений Розбір:

Дати в текстовому форматі не завжди легко обробити. Скажімо, у ранніх версіях PHP парсинг дат був складніший і призводив до помилок через обмежене розуміння форматів. Функція `strtotime()` була одним з перших рішень, але вона не ідеально працювала з нетиповими форматами.

Спільнота PHP відповіла на це створенням об'єктно-орієнтованого класу `DateTime`, який надав розробникам більш гнучкі можливості. З `DateTime::createFromFormat`, ви можете вказати точний формат дати, а PHP правильно її обробить.

Але навіщо все це? Уявіть, вам треба працювати з користувацькими датами у різних форматах, зі сторонніх API, або просто перетворювати строки у дати для збереження в базі даних. Тут `DateTime` стане у нагоді.

Ще однією альтернативою є використання функцій `date_parse()` або `date_parse_from_format()`, які повертають масив із компонентами дати, натомість створення об'єкту `DateTime`.

## Дивіться Ще:

- Офіційна документація по класу `DateTime`: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Функція `strtotime()`: [php.net/manual/en/function.strtotime.php](https://www.php.net/manual/en/function.strtotime.php)
- Функція `date_parse()`: [php.net/manual/en/function.date-parse.php](https://www.php.net/manual/en/function.date-parse.php)
- Функція `date_parse_from_format()`: [php.net/manual/en/function.date-parse-from-format.php](https://www.php.net/manual/en/function.date-parse-from-format.php)
- Порівняння різних методів парсингу дат у PHP: [stackoverflow.com/questions](https://stackoverflow.com/questions) (шукайте "PHP date parsing" або "PHP DateTime vs strtotime").