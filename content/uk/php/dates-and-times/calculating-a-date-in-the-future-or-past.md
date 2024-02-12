---
title:                "Обчислення дати у майбутньому або минулому"
aliases: - /uk/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:52.231173-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Обчислення дати у майбутньому чи минулому - це визначення точної дати на основі заданого інтервалу часу. Програмісти роблять це для функціоналу планувальників, нагадувань, та обробки даних, залежних від часу.

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
