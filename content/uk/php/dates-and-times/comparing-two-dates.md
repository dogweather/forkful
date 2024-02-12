---
title:                "Порівняння двох дат"
aliases:
- uk/php/comparing-two-dates.md
date:                  2024-01-20T17:33:52.128458-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат — це процес визначення їх відносної послідовності у часі. Програмісти роблять це, щоб керувати логікою роботи програм, термінами дій та сортуванням історичних даних.

## Як зробити:
```php
<?php
$date1 = new DateTime("2023-03-10");
$date2 = new DateTime("2023-04-01");

if ($date1 < $date2) {
    echo "Дата1 раніше за Дату2.";
} elseif ($date1 > $date2) {
    echo "Дата1 пізніше за Дату2.";
} else {
    echo "Дата1 та Дата2 ідентичні.";
}
// Вивід: Дата1 раніше за Дату2.
```

## Глибше занурення:
Порівняння дат у PHP набуло розвитку із запровадженням об'єктно-орієнтованого підходу у DateTime. Історично було багато способів (наприклад, strtotime, mktime), але DateTime став стандартом через легкість використання та об'єктні можливості оперування датами.

DateTime пропонує методи порівняння через оператори (>, <, ==). Альтернативний підхід може включати DateTime::diff, який віддає DateInterval для точного розрахунку різниці між двома датами.

Важливо врахувати часові зони при порівнянні міжнародних дат. Уникайте використання строкових порівнянь, бо формати дат можуть варіюватися і призводити до помилок.

## Див. також:
- [PHP DateTime клас](https://www.php.net/manual/en/class.datetime.php)
- [DateInterval для розрахунку різниці](https://www.php.net/manual/en/class.dateinterval.php)
- [Функція strtotime()](https://www.php.net/manual/en/function.strtotime.php)
