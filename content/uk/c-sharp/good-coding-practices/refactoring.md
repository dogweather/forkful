---
date: 2024-01-26 01:18:12.119094-07:00
description: "\u042F\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u043D\u0430\u0454\u043C\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\
  \u0440\u0438\u043D\u0433 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E \u043C\
  \u0435\u0442\u043E\u0434\u0443 C#, \u044F\u043A\u0438\u0439 \u043E\u0431\u0447\u0438\
  \u0441\u043B\u044E\u0454 \u0456 \u0432\u0438\u0432\u043E\u0434\u0438\u0442\u044C\
  \ \u0441\u0443\u043C\u0443 \u043C\u0430\u0441\u0438\u0432\u0443 \u0447\u0438\u0441\
  \u0435\u043B: \u0414\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\
  \u043D\u0433\u0443."
lastmod: '2024-03-13T22:44:49.303417-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u0438\u043A\u043E\u043D\
  \u0430\u0454\u043C\u043E \u0440\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\
  \u0433 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E \u043C\u0435\u0442\u043E\
  \u0434\u0443 C#, \u044F\u043A\u0438\u0439 \u043E\u0431\u0447\u0438\u0441\u043B\u044E\
  \u0454 \u0456 \u0432\u0438\u0432\u043E\u0434\u0438\u0442\u044C \u0441\u0443\u043C\
  \u0443 \u043C\u0430\u0441\u0438\u0432\u0443 \u0447\u0438\u0441\u0435\u043B."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Як:
Давайте виконаємо рефакторинг простого методу C#, який обчислює і виводить суму масиву чисел:

До рефакторингу:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("Сума становить " + sum);
    }
}
```

Після рефакторингу:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"Сума становить {CalculateSum()}");
    }
}

// Використання:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Завдяки рефакторингу ми розділили відповідальності, зробили клас `Calculator` гнучкішим, дозволивши йому приймати будь-який масив чисел, та використали LINQ для того, щоб обчислення суми стало більш лаконічним.

## Поглиблений огляд
Рефакторинг має своє коріння в спільноті програмістів Smalltalk і став популярним у 1990-х роках завдяки книзі Мартіна Фаулера "Рефакторинг: покращення дизайну існуючого коду". З тих пір це стало фундаментальною частиною агільних методологій та хороших практик написання коду.

Існують різні підходи до рефакторингу, такі як Червоний-Зелений-Рефактор у Розробці через Тестування (TDD). Це гарантує, що рефакторинг не вводить помилки, починаючи з невдалих тестів, роблячи їх успішними, а потім очищаючи код.

При проведенні рефакторингу важливо мати повний набір тестів, щоб переконатися, що функціональність не порушується під час процесу. Автоматизовані інструменти рефакторингу, як-от ReSharper для C#, також можуть допомогти в цьому процесі, надаючи безпечні способи зміни структур коду. Однак, інструменти повинні бути доповненням до глибокого розуміння кодової бази та принципів кодування.

## Див. також
- Фундаментальна праця Мартіна Фаулера про Рефакторинг: [Рефакторинг: покращення дизайну існуючого коду](https://martinfowler.com/books/refactoring.html)
- Посібник Microsoft з Рефакторингу в Visual Studio: [Рефакторинг (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Детальний огляд патернів рефакторингу з прикладами: [Рефакторинг на SourceMaking](https://sourcemaking.com/refactoring)
