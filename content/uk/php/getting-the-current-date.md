---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:16:14.385753-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Отримання поточної дати в PHP – це про витяг дати та часу прямо зараз. Програмісти роблять це для логування, тайм-стемпів, функцій часових розрахунків.

## Як це зробити:
В PHP для цього є функція `date()`. Щоб отримати повну дату й час:

```php
echo date('Y-m-d H:i:s');
```

Це виглядатиме приблизно так: `2023-04-01 12:45:30`.

Тільки дата:

```php
echo date('Y-m-d');
```

Виходить таке: `2023-04-01`.

А ось що для часу:

```php
echo date('H:i:s');
```

Покаже: `12:45:30`.

## Поглиблені знання:
`date()` базується на часі сервера (втім, це можна змінити). Початкова версія PHP мала цю функцію від самого старту. В PHP 5.2.0 з'явилися класи `DateTime` та `DateTimeZone` з більш потужними можливостями, як от робота з часовими поясами.

Alternatives? `DateTime`! Ось як це в роботі:

```php
$now = new DateTime();
echo $now->format('Y-m-d H:i:s');
```

Гнучкість `$now` дозволяє маніпулювати датами і часом значно легше.

Ще один момент: перевірте часовий пояс (`date_default_timezone_get()`). Якщо потрібно, змініть його за допомогою `date_default_timezone_set('Europe/Kiev')`.

## Дивіться ще:
- [Функція date() в PHP](https://www.php.net/manual/en/function.date.php)
- [Клас DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Часові пояси](https://www.php.net/manual/en/timezones.php)
- [Stack Overflow: PHP date() vs DateTime](https://stackoverflow.com/questions/4160357/php-date-vs-datetime)