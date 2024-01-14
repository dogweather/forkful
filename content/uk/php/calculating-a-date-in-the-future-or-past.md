---
title:                "PHP: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Калькулювання дати в майбутнє або минуле може бути корисним для створення розкладів, визначення моменту закінчення проекту або розрахунку терміну дострокової оплати за послуги.

## Як

```PHP
// створення нового об'єкта DateTime з поточною датою
$now = new DateTime();

// отримання дати через 1 рік
$oneYearFromNow = $now->modify('+1 year')->format('d-m-Y');

// отримання дати 3 місяці назад
$threeMonthsAgo = $now->modify('-3 months')->format('d-m-Y');

// отримання дати через 2 тижні
$twoWeeksFromNow = $now->modify('+2 weeks')->format('d-m-Y');

echo "Через 1 рік: $oneYearFromNow <br>"; 
echo "3 місяці назад: $threeMonthsAgo <br>";
echo "Через 2 тижні: $twoWeeksFromNow <br>";
```

Вище наведені приклади демонструють як створити новий об'єкт DateTime з поточною датою та як можна змінити цю дату на потрібну. Для калькулювання дати в майбутнє або минуле, можна використовувати методи `modify()` та `format()` для отримання необхідного формату дати.

## Глибоке дослідження

Клас DateTime має багато корисних методів для роботи з датами. Наприклад, можна використовувати метод `add()` для додавання додаткових днів, місяців або років до поточної дати. Крім того, можна використовувати метод `diff()` для визначення різниці між двома датами та метод `setTimezone()` для задання необхідної часової зони.

Також існують бібліотеки та фреймворки, які пропонують ще більше зручних функцій для роботи з датами, наприклад Carbon для Laravel або DateTime для Symfony.

## Дивіться також

- [Метод `modify()` класу DateTime](https://www.php.net/manual/uk/datetime.modify.php)
- [Форматування дат в PHP](https://www.php.net/manual/uk/datetime.format.php)
- [Бібліотека Carbon для Laravel](https://carbon.nesbot.com/)
- [ Клас DateTime для Symfony](https://symfony.com/doc/current/components/datetime.html)