---
title:                "Округление чисел"
date:                  2024-01-29T00:01:53.089726-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Округление чисел означает их корректировку до ближайшего указанного разряда — это как упрощение формы. Программисты округляют для контроля точности, повышения производительности или при отображении дружелюбных к пользователю результатов — например, цен, которым не нужно три десятичных знака.

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
