---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:04.876215-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043E `DateTime` \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0438\u0441\u0442\u044B \u043F\u043E\u043B\u0430\u0433\u0430\u043B\u0438\u0441\
  \u044C \u043D\u0430 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\
  \u043B\u044C\u0441\u043A\u0438\u0439 \u043A\u043E\u0434 \u0434\u043B\u044F \u0440\
  \u0430\u0431\u043E\u0442\u044B \u0441 \u0434\u0430\u0442\u0430\u043C\u0438, \u0447\
  \u0442\u043E \u0447\u0440\u0435\u0432\u0430\u0442\u043E \u043E\u0448\u0438\u0431\
  \u043A\u0430\u043C\u0438 \u0438 \u043D\u0435\u044D\u0444\u0444\u0435\u043A\u0442\
  \u0438\u0432\u043D\u043E\u0441\u0442\u044C\u044E. \u0421\u0442\u0440\u0443\u043A\
  \u0442\u0443\u0440\u0430\u2026"
lastmod: '2024-04-05T21:53:45.570652-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043E `DateTime` \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\
  \u0438\u0441\u0442\u044B \u043F\u043E\u043B\u0430\u0433\u0430\u043B\u0438\u0441\u044C\
  \ \u043D\u0430 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\
  \u044C\u0441\u043A\u0438\u0439 \u043A\u043E\u0434 \u0434\u043B\u044F \u0440\u0430\
  \u0431\u043E\u0442\u044B \u0441 \u0434\u0430\u0442\u0430\u043C\u0438, \u0447\u0442\
  \u043E \u0447\u0440\u0435\u0432\u0430\u0442\u043E \u043E\u0448\u0438\u0431\u043A\
  \u0430\u043C\u0438 \u0438 \u043D\u0435\u044D\u0444\u0444\u0435\u043A\u0442\u0438\
  \u0432\u043D\u043E\u0441\u0442\u044C\u044E."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как это сделать:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023-03-15";
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate); // Вывод: 3/15/2023 12:00:00 AM
        
        // С конкретным форматом
        dateString = "15 марта, 2023";
        string format = "d MMMM, yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;
        parsedDate = DateTime.ParseExact(dateString, format, provider);
        Console.WriteLine(parsedDate); // Вывод: 3/15/2023 12:00:00 AM
    }
}
```

## Подробное погружение
До `DateTime` программисты полагались на пользовательский код для работы с датами, что чревато ошибками и неэффективностью. Структура `DateTime` в .NET стала настоящей революцией в этом вопросе, предоставив надежные методы разбора — `Parse` и `ParseExact`.

`Parse` пытается понять строку с датой, основываясь на специфических для культуры или универсальных форматах. Отлично подходит, когда вы ожидаете стандартные форматы дат. Однако, если у вас есть конкретные или нестандартные форматы дат, `ParseExact` (вместе с `TryParse` и `TryParseExact` для обработки ошибок) будет вашим спасением. Здесь вы указываете точный формат с помощью пользовательского шаблона.

Реализация использует класс `CultureInfo` для учета различных культурных форматов дат. Используя `ParseExact`, вы избегаете культурных недопониманий — ваш определенный шаблон и есть закон. Помните, компьютерные даты начинаются с 1 января 0001 года, поэтому убедитесь, что ваша строка представляет действительную дату в пределах календарного диапазона .NET.

## Смотри также
- [Документация по методу DateTime.Parse](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime.parse)
- [Документация по методу DateTime.ParseExact](https://docs.microsoft.com/ru-ru/dotnet/api/system.datetime.parseexact)
- [Пользовательские строковые форматы даты и времени](https://docs.microsoft.com/ru-ru/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Класс CultureInfo](https://docs.microsoft.com/ru-ru/dotnet/api/system.globalization.cultureinfo)
