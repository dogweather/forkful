---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:13.404807-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0425\u043E\u0442\u0438\u0442\u0435 \u043F\u043E\u043B\u0443\u0447\
  \u0438\u0442\u044C \u0442\u0435\u043A\u0443\u0449\u0443\u044E \u0434\u0430\u0442\
  \u0443? \u041F\u0440\u043E\u0441\u0442\u043E \u0432\u044B\u0437\u043E\u0432\u0438\
  \u0442\u0435 `DateTime.Now`. \u0412\u043E\u0442 \u043F\u0440\u0438\u043C\u0435\u0440\
  \ \u043A\u043E\u0434\u0430."
lastmod: '2024-03-13T22:44:45.075586-06:00'
model: gpt-4-0125-preview
summary: "\u0425\u043E\u0442\u0438\u0442\u0435 \u043F\u043E\u043B\u0443\u0447\u0438\
  \u0442\u044C \u0442\u0435\u043A\u0443\u0449\u0443\u044E \u0434\u0430\u0442\u0443\
  ."
title: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0443\
  \u0449\u0435\u0439 \u0434\u0430\u0442\u044B"
weight: 29
---

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
