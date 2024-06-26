---
date: 2024-01-26 03:44:01.485084-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: \u041E\u0441\
  \u044C \u043F\u043E\u0441\u0456\u0431\u043D\u0438\u043A \u0434\u043B\u044F \u043E\
  \u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\u0435\u043B\
  \ \u0443 C#."
lastmod: '2024-03-13T22:44:49.280782-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u044C \u043F\u043E\u0441\u0456\u0431\u043D\u0438\u043A \u0434\
  \u043B\u044F \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\
  \u0438\u0441\u0435\u043B \u0443 C#."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Як зробити:
Ось посібник для округлення чисел у C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Округлення до найближчого цілого числа
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Вивід: 123

        // Вказання кількості десяткових знаків
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Вивід: 123.46

        // Округлення вгору незалежно від наступної цифри
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Вивід: 124

        // Округлення вниз незалежно від наступної цифри
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Вивід: 123
    }
}
```

## Поглиблений огляд
У старі часи округлення було очевидним рішенням для скорочення обчислювальних витрат. Кожен цикл мав значення, і скорочення чисел економило дорогоцінний час. Прискорившись до сучасного C#, йдеться про управління double і decimal, відомими своєю схильністю до помилок точності та особливостей відображення.

Окрім `Math.Round`, `Math.Floor`, і `Math.Ceiling`, перелік `MidpointRounding` дозволяє нам вирішувати долю бідних, середніх чисел—це перехрестя між банківськими правилами і справедливістю дитячого майданчика "округлити вгору".

Для складніших аудиторій, як от серйозні математичні чи фінансові застосування, ми маємо `decimal` замість `double`, зменшуючи драму округлення завдяки вищій точності—менше округлень, менше проблем.

## Дивіться також
- [Офіційна документація C# про `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Коли мені слід використовувати Double замість Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Стандарт IEEE для арифметики з плаваючою комою (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
