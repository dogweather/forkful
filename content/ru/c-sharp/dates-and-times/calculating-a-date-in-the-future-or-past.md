---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:58.201227-07:00
description: "\u041A\u0430\u043A: \u0412 C#, `DateTime` \u0438 `TimeSpan` \u044F\u0432\
  \u043B\u044F\u044E\u0442\u0441\u044F \u043E\u0441\u043D\u043E\u0432\u043E\u0439\
  \ \u0434\u043B\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0438\u0439 \u0441 \u0434\
  \u0430\u0442\u0430\u043C\u0438 \u0438 \u0432\u0440\u0435\u043C\u0435\u043D\u0435\
  \u043C. `DateTime` \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0435\
  \u0442 \u043C\u043E\u043C\u0435\u043D\u0442 \u0432\u0440\u0435\u043C\u0435\u043D\
  \u0438, \u043E\u0431\u044B\u0447\u043D\u043E \u0432\u044B\u0440\u0430\u0436\u0435\
  \u043D\u043D\u044B\u0439 \u0432 \u0432\u0438\u0434\u0435\u2026"
lastmod: '2024-04-05T22:50:58.559346-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C#, `DateTime` \u0438 `TimeSpan` \u044F\u0432\u043B\u044F\u044E\u0442\
  \u0441\u044F \u043E\u0441\u043D\u043E\u0432\u043E\u0439 \u0434\u043B\u044F \u043E\
  \u043F\u0435\u0440\u0430\u0446\u0438\u0439 \u0441 \u0434\u0430\u0442\u0430\u043C\
  \u0438 \u0438 \u0432\u0440\u0435\u043C\u0435\u043D\u0435\u043C."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

## Как:
Вычисление будущих дат:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan oneWeek = TimeSpan.FromDays(7);
        
        DateTime nextWeek = currentDate + oneWeek;
        Console.WriteLine($"Через неделю будет: {nextWeek}");
    }
}
```

Вывод:

```
Через неделю будет: <дата через неделю от текущей даты>
```

Вычисление прошлых дат:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan tenDaysAgo = TimeSpan.FromDays(-10);
        
        DateTime pastDate = currentDate + tenDaysAgo;
        Console.WriteLine($"Десять дней назад было: {pastDate}");
    }
}
```

Вывод:

```
Десять дней назад было: <дата десять дней до текущей даты>
```

## Подробнее
В C#, `DateTime` и `TimeSpan` являются основой для операций с датами и временем. `DateTime` представляет момент времени, обычно выраженный в виде даты и времени суток. `TimeSpan` представляет временной интервал.

Исторически расчеты дат и времени были подвержены ошибкам из-за ручного управления днями, месяцами и високосными годами. `DateTime` абстрагирует эти сложности, позволяя фреймворку обрабатывать сложные моменты.

Альтернативы `DateTime` и `TimeSpan` в .NET включают `DateTimeOffset`, который включает смещение часового пояса, что делает его лучше для приложений, работающих в разных часовых поясах. Другая альтернатива - это Noda Time, библиотека от Джона Скита, предназначенная для более сложной обработки дат и времени, например, для различных календарей.

С точки зрения реализации, когда вы добавляете `TimeSpan` к `DateTime`, под капотом происходит манипулирование тиками, фундаментальной единицей времени в .NET (`1 тик = 100 наносекунд`). Для прошлых дат подойдет отрицательный `TimeSpan`.

## Смотрите также
- Документация .NET API для [`DateTime`](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime)
- Введение в [`TimeSpan`](https://docs.microsoft.com/ru-ru/dotnet/api/system.timespan)
- Лучшие практики Microsoft для [`DateTime` и `DateTimeOffset`](https://docs.microsoft.com/ru-ru/dotnet/standard/datetime/choosing-between-datetime)
- Документация Noda Time: [https://nodatime.org](https://nodatime.org)
