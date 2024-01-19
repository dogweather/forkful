---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому? 
Порівняння двох дат - це процес визначення їх взаємного ставлення у часовій послідовності. Програмісти використовують це для обробки елементів, що залежать від дати (наприклад, розкладу подій, обмежень терміну дії або системи архівування).

## Як: 
```PHP
<?php
$date1=date_create("2023-03-15");
$date2=date_create("2023-12-12");
$diff=date_diff($date1,$date2);
echo $diff->format("%R%a days");
?>
```

Цей код виведе різницю в днях між 15 березня 2023 року та 12 грудня 2023 року.

## Поглиблений аналіз:
Раніше дати порівнювали за допомогою таймштампів UNIX, але це метод орієнтований на переклад дати в секунди, а не на пряме порівняння. Дати можна також порівнювати як рядки, але в такому випадку вам слід подбати про форматування, щоб впевнитися, що вони правильно порівнюються.

Порівняння дат за допомогою PHP DateTime об'єктів - найбільш гнучкий спосіб, що забезпечує точне і надійне порівняння. Ви також можете порівнювати час, що дає додаткові можливості.

## Дивіться також:
[PHP документація по DateTime](https://www.php.net/manual/en/class.datetime.php)
[Туторіал по порівнянню дат в PHP](https://www.phptutorial.net/php-tutorial/php-compare-dates/)
[Пост на StackOverflow про порівняння дат](https://stackoverflow.com/questions/1290318/php-how-do-i-compare-two-datetime-objects)