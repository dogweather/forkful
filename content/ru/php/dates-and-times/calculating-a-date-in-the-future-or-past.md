---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:08.371239-07:00
description: "\u041A\u0430\u043A: PHP \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442\
  \ \u0440\u0430\u0431\u043E\u0442\u0443 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438\
  \ \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E `DateTime` \u0438 `DateInterval`.\
  \ \u041F\u043E\u0441\u043C\u043E\u0442\u0440\u0438\u0442\u0435."
lastmod: '2024-03-13T22:44:45.235531-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u0440\u0430\u0431\u043E\
  \u0442\u0443 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438 \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E `DateTime` \u0438 `DateInterval`."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

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
