---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:53.089726-07:00
description: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\
  \u0441\u0435\u043B \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0445\
  \ \u043A\u043E\u0440\u0440\u0435\u043A\u0442\u0438\u0440\u043E\u0432\u043A\u0443\
  \ \u0434\u043E \u0431\u043B\u0438\u0436\u0430\u0439\u0448\u0435\u0433\u043E \u0443\
  \u043A\u0430\u0437\u0430\u043D\u043D\u043E\u0433\u043E \u0440\u0430\u0437\u0440\u044F\
  \u0434\u0430 \u2014 \u044D\u0442\u043E \u043A\u0430\u043A \u0443\u043F\u0440\u043E\
  \u0449\u0435\u043D\u0438\u0435 \u0444\u043E\u0440\u043C\u044B. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u043E\u043A\u0440\u0443\u0433\
  \u043B\u044F\u044E\u0442 \u0434\u043B\u044F \u043A\u043E\u043D\u0442\u0440\u043E\
  \u043B\u044F \u0442\u043E\u0447\u043D\u043E\u0441\u0442\u0438,\u2026"
lastmod: '2024-03-13T22:44:45.046949-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\
  \u0441\u0435\u043B \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442 \u0438\u0445\
  \ \u043A\u043E\u0440\u0440\u0435\u043A\u0442\u0438\u0440\u043E\u0432\u043A\u0443\
  \ \u0434\u043E \u0431\u043B\u0438\u0436\u0430\u0439\u0448\u0435\u0433\u043E \u0443\
  \u043A\u0430\u0437\u0430\u043D\u043D\u043E\u0433\u043E \u0440\u0430\u0437\u0440\u044F\
  \u0434\u0430 \u2014 \u044D\u0442\u043E \u043A\u0430\u043A \u0443\u043F\u0440\u043E\
  \u0449\u0435\u043D\u0438\u0435 \u0444\u043E\u0440\u043C\u044B."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
Вот кругосветное путешествие по округлению чисел в C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Округление до ближайшего целого числа
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Вывод: 123

        // Указываем количество десятичных знаков
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Вывод: 123.46

        // Округление в большую сторону независимо от следующей цифры
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Вывод: 124

        // Округление в меньшую сторону независимо от следующей цифры
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Вывод: 123
    }
}
```

## Глубокое погружение
В старые времена округление было очевидным способом сокращения вычислительных затрат. Каждый цикл имел значение, и урезание чисел экономило драгоценное время. Дошли до современного C#, и речь идет о управлении печально известной предрасположенностью `double` и `decimal` к ошибкам точности и отображения.

Помимо `Math.Round`, `Math.Floor` и `Math.Ceiling`, перечисление `MidpointRounding` позволяет нам решать судьбу несчастных, сидящих посередине цифр — это перекресток между банковскими правилами и справедливостью игровой площадки "округлить к половине вверх".

Для более требовательных задач, например, в серьезных математических или финансовых приложениях, мы выбираем `decimal` вместо `double`, сокращая драму округления за счет более высокой точности — меньше округлений, меньше проблем.

## См. также
- [Официальная документация C# по `Math.Round`](https://docs.microsoft.com/ru-ru/dotnet/api/system.math.round)
- [Stack Overflow: Когда мне следует использовать Double вместо Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Стандарт IEEE для арифметики с плавающей запятой (IEEE 754)](https://ru.wikipedia.org/wiki/IEEE_754)
