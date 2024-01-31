---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:55:58.201227-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Вычисление будущей или прошлой даты включает в себя определение, какой была или будет дата после или до определенного временного интервала. Программистам это часто требуется для планирования событий, управления сроками действия или записи данных, зависящих от времени.

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
