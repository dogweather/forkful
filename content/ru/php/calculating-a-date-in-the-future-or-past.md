---
title:                "Расчет даты в будущем или прошлом"
aliases:
- ru/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T23:56:08.371239-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Расчёт будущей или прошлой даты означает нахождение даты до или после указанного времени. Программисты делают это для напоминаний, подписок, планирования и множества других функций, связанных со временем, в приложениях.

## Как:
PHP упрощает работу с датами с помощью `DateTime` и `DateInterval`. Посмотрите:

```PHP
<?php
// Сегодняшняя дата
$today = new DateTime();
echo $today->format('Y-m-d H:i:s') . "\n";

// Добавить 10 дней
$today->add(new DateInterval('P10D'));
echo $today->format('Y-m-d H:i:s') . "\n";

// Вычесть 2 месяца
$today->sub(new DateInterval('P2M'));
echo $today->format('Y-m-d H:i:s') . "\n";
?>
```
Вывод может быть таким:
```
2023-04-01 12:34:56
2023-04-11 12:34:56
2023-02-11 12:34:56
```

## Глубже
В прошлом расчёты дат в PHP были более подвержены ошибкам. `strtotime`, хотя и по-прежнему полезен, может подвести в крайних случаях. `DateTime` и `DateInterval` принесли точность и ясность, ориентированную на объекты.

Альтернативы? Конечно. Библиотеки вроде Carbon оборачивают функциональность дат в PHP для большей читабельности и функционала, но во многих случаях встроенных классов PHP будет вполне достаточно.

Под капотом, `DateTime::add()` и `DateTime::sub()` изменяют объект, так что нет необходимости в повторном присвоении. Они обрабатывают временные единицы последовательно, учитывая такие вещи, как високосные года и изменения времени из-за перехода на летнее/зимнее время, что в противном случае могло бы быть настоящей головной болью.

## Смотрите также
- Руководство PHP по DateTime: https://www.php.net/manual/en/class.datetime.php
- Документация DateInterval: https://www.php.net/manual/en/class.dateinterval.php
- Carbon: Простое API-расширение для DateTime - https://carbon.nesbot.com
