---
title:                "Округлення чисел"
date:                  2024-01-26T03:44:01.485084-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел означає їх коригування до найближчого вказаного розряду— думайте про це як про спрощення їх форми. Програмісти заокруглюють для контролю точності, підвищення продуктивності або при виведенні дружніх до користувача результатів—як от ціни, яким не потрібно трьох десяткових знаків.

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