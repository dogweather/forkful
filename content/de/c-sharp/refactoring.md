---
title:                "Refactoring"
date:                  2024-01-26T01:17:36.958004-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes, ohne dessen äußeres Verhalten zu verändern. Programmierer tun dies, um den Code aufzuräumen, die Lesbarkeit zu erhöhen, die Komplexität zu reduzieren und die Wartbarkeit zu verbessern.

## Wie geht das:

Lassen Sie uns eine einfache C#-Methode refaktorisieren, die die Summe eines Arrays von Zahlen berechnet und ausgibt:

Vor dem Refactoring:
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
        Console.WriteLine("Die Summe ist " + sum);
    }
}
```

Nach dem Refactoring:
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
        Console.WriteLine($"Die Summe ist {CalculateSum()}");
    }
}

// Verwendung:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Durch das Refactoring haben wir Bedenken getrennt, die `Calculator`-Klasse flexibler gemacht, indem sie es ermöglicht, jedes Array von Zahlen zu nehmen, und LINQ genutzt, um die Summierungsberechnung prägnanter zu machen.

## Tiefergehende Betrachtung

Refactoring hat seine Wurzeln in der Smalltalk-Programmiergemeinschaft und wurde in den 1990er Jahren durch Martin Fowlers Buch "Refactoring: Improving the Design of Existing Code" populär. Im Laufe der Jahre ist es ein fundamentaler Teil der agilen Methodologien und guter Programmierpraktiken geworden.

Es gibt verschiedene Ansätze zum Refactoring, wie zum Beispiel Red-Green-Refactor in der testgetriebenen Entwicklung (TDD). Dies stellt sicher, dass durch Refactoring keine Bugs eingeführt werden, indem man mit einem fehlschlagenden Test beginnt, diesen zum Laufen bringt und dann den Code aufräumt.

Bei der Implementierung des Refactoring ist es entscheidend, eine umfassende Testsuite zu haben, um sicherzustellen, dass während des Prozesses keine Funktionalität kaputt geht. Automatisierte Refactoring-Tools, wie ReSharper für C#, können diesen Prozess unterstützen, indem sie sichere Möglichkeiten zur Änderung von Code-Strukturen bieten. Allerdings sollte das Tooling ergänzend zu einem tiefen Verständnis der Codebasis und der Programmierprinzipien sein.

## Siehe auch

- Martin Fowlers grundlegendes Werk über Refactoring: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Microsofts Anleitung zum Refactoring in Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Ein detaillierter Blick auf Refactoring-Muster mit Beispielen: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)