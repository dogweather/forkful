---
date: 2024-01-20 17:33:52.128458-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u041F\u043E\
  \u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0430\u0442 \u0443 PHP \u043D\
  \u0430\u0431\u0443\u043B\u043E \u0440\u043E\u0437\u0432\u0438\u0442\u043A\u0443\
  \ \u0456\u0437 \u0437\u0430\u043F\u0440\u043E\u0432\u0430\u0434\u0436\u0435\u043D\
  \u043D\u044F\u043C \u043E\u0431'\u0454\u043A\u0442\u043D\u043E-\u043E\u0440\u0456\
  \u0454\u043D\u0442\u043E\u0432\u0430\u043D\u043E\u0433\u043E \u043F\u0456\u0434\u0445\
  \u043E\u0434\u0443 \u0443 DateTime. \u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\
  \u043E \u0431\u0443\u043B\u043E \u0431\u0430\u0433\u0430\u0442\u043E \u0441\u043F\
  \u043E\u0441\u043E\u0431\u0456\u0432 (\u043D\u0430\u043F\u0440\u0438\u043A\u043B\
  \u0430\u0434,\u2026"
lastmod: '2024-04-05T21:53:49.614117-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0430\
  \u0442 \u0443 PHP \u043D\u0430\u0431\u0443\u043B\u043E \u0440\u043E\u0437\u0432\u0438\
  \u0442\u043A\u0443 \u0456\u0437 \u0437\u0430\u043F\u0440\u043E\u0432\u0430\u0434\
  \u0436\u0435\u043D\u043D\u044F\u043C \u043E\u0431'\u0454\u043A\u0442\u043D\u043E\
  -\u043E\u0440\u0456\u0454\u043D\u0442\u043E\u0432\u0430\u043D\u043E\u0433\u043E\
  \ \u043F\u0456\u0434\u0445\u043E\u0434\u0443 \u0443 DateTime."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

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
