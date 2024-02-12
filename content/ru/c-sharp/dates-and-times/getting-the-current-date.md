---
title:                "Получение текущей даты"
aliases: - /ru/c-sharp/getting-the-current-date.md
date:                  2024-01-28T23:58:13.404807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Получение текущей даты в C# заключается в извлечении текущего момента из системных часов. Это полезно для временных меток, логов или любой функции, требующей проверки даты.

## Как это сделать:

Хотите получить текущую дату? Просто вызовите `DateTime.Now`. Вот пример кода:

```C#
using System;

class GetCurrentDate
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```

Если вы запустите его, ожидайте что-то вроде этого:

```
25.03.2023 11:34:52
```

Неплохо, правда?

## Подробнее

До `DateTime` программисты жонглировали датами-временем в уме. Теперь .NET упрощает это. `DateTime.Now` захватывает и дату, и время, но для получения только даты существует `DateTime.Today`.

Вот особенность – учитываются часовые пояса. `DateTime.UtcNow` дает вам Всемирное координированное время (UTC), избегая проблем с местным временем.

Исторически, учет времени был хаотичен – подумайте о солнечных часах, водяных часах, и так далее. Компьютеры упростили это, но часовые пояса и правила сезонного перевода времени все еще усложняют вещи. К счастью, в C# есть `TimeZoneInfo`, если вам нужно маневрировать с часовыми поясами.

Помимо `DateTime`, у нас есть `DateTimeOffset`. Он сочетает дату-время с смещением от UTC, полезно, если вам важна специфика часовых поясов.

С точки зрения реализации, `DateTime` в C# точен до 100-наносекундных тиков с полуночи 1 января 0001 года н.э. Но не планируйте свои наносекунды вокруг этого – точность и допустимые погрешности системных часов сильно различаются.

## Смотрите также

- [Структура DateTime](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime?view=net-7.0)
- [Свойство DateTime.UtcNow](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime.utcnow?view=net-7.0)
- [Структура DateTimeOffset](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetimeoffset?view=net-7.0)
- [Класс TimeZoneInfo](https://docs.microsoft.com/ru-ru/dotnet/api/system.timezoneinfo?view=net-7.0)
