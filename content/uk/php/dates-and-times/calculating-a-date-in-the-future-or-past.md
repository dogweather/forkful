---
date: 2024-01-20 17:31:52.231173-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 PHP \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0432 \u0444\u0443\u043D\
  \u043A\u0446\u0456\u0457 `strtotime()` \u0456 `date()` \u0434\u043B\u044F \u0440\
  \u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438. \u0417\
  \ \u0447\u0430\u0441\u043E\u043C \u0437'\u044F\u0432\u0438\u043B\u0438\u0441\u044F\
  \ \u043E\u0431'\u0454\u043A\u0442\u043D\u043E-\u043E\u0440\u0456\u0454\u043D\u0442\
  \u043E\u0432\u0430\u043D\u0456 \u043A\u043B\u0430\u0441\u0438 `DateTime`\u2026"
lastmod: '2024-04-05T21:53:49.615393-06:00'
model: gpt-4-1106-preview
summary: "\u0412 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 PHP \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0432 \u0444\u0443\
  \u043D\u043A\u0446\u0456\u0457 `strtotime()` \u0456 `date()` \u0434\u043B\u044F\
  \ \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0434\u0430\u0442\u0430\u043C\u0438\
  ."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
```PHP
<?php
// Сьогоднішня дата
$today = new DateTime();

// Додавання 10 днів
$futureDate = clone $today;
$futureDate->modify('+10 days');
echo $futureDate->format('Y-m-d') . PHP_EOL; // Приклад виводу: 2023-04-10

// Віднімання 1 місяця
$pastDate = clone $today;
$pastDate->modify('-1 month');
echo $pastDate->format('Y-m-d') . PHP_EOL; // Приклад виводу: 2023-03-01
?>
```

## Поглиблено:
В минулому PHP використовував функції `strtotime()` і `date()` для роботи з датами. З часом з'явилися об'єктно-орієнтовані класи `DateTime` і `DateInterval`, що дозволили більш гнучку та зрозумілу роботу з часом.

Альтернативою є використання бібліотек, таких як Carbon для PHP, яка надає ще більше функціоналу.

Особливості реалізації обчислення дати включають в себе коректне врахування переходу на літній/зимовий час і роботу з різними часовими зонами.

## Дивись також:
- [PHP Manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [DateTimeImmutable](https://www.php.net/manual/en/class.datetimeimmutable.php) - аналог `DateTime` з незмінними об'єктами.
- [Carbon - A simple PHP API extension for DateTime](https://carbon.nesbot.com/)
