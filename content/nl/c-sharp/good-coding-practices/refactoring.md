---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:59.079491-07:00
description: "Refactoring is het proces van het herstructureren van bestaande computercodes\
  \ zonder het externe gedrag ervan te wijzigen. Programmeurs doen dit om code\u2026"
lastmod: '2024-03-13T22:44:50.818632-06:00'
model: gpt-4-0125-preview
summary: "Refactoring is het proces van het herstructureren van bestaande computercodes\
  \ zonder het externe gedrag ervan te wijzigen. Programmeurs doen dit om code\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring is het proces van het herstructureren van bestaande computercodes zonder het externe gedrag ervan te wijzigen. Programmeurs doen dit om code op te schonen, leesbaarheid te verbeteren, complexiteit te verminderen en onderhoudbaarheid te verbeteren.

## Hoe te:

Laten we een eenvoudige C# methode refactoren die de som van een reeks getallen berekent en afdrukt:

Voor Refactoring:
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
        Console.WriteLine("De som is " + sum);
    }
}
```

Na Refactoring:
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
        Console.WriteLine($"De som is {CalculateSum()}");
    }
}

// Gebruik:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Door te refactoren hebben we de zorgen gescheiden, de `Calculator` klasse flexibeler gemaakt door het mogelijk te maken om elk willekeurig aantal reeksen te gebruiken, en hebben we LINQ gebruikt om de somberekening beknopter te maken.

## Diepere Duik

Refactoring vindt zijn oorsprong in de Smalltalk-programmeercommunity en werd in de jaren '90 populair gemaakt door het boek van Martin Fowler "Refactoring: Improving the Design of Existing Code". Door de jaren heen is het een fundamenteel onderdeel geworden van agile methodologieën en goede programmeerpraktijken.

Er zijn verschillende benaderingen van refactoring, zoals Red-Green-Refactor in Test-Driven Development (TDD). Dit zorgt ervoor dat refactoring geen bugs introduceert door te beginnen met een falende test, deze te laten slagen en vervolgens de code op te ruimen.

Bij het implementeren van refactoring is het cruciaal om een uitgebreide testsuite te hebben om ervoor te zorgen dat er geen functionaliteit wordt gebroken tijdens het proces. Geautomatiseerde refactoringtools, zoals ReSharper voor C#, kunnen ook helpen in dit proces door veilige manieren te bieden om codestructuren te veranderen. Echter, tooling zou aanvullend moeten zijn op een diepgaand begrip van de codebasis en programmeerprincipes.

## Zie Ook

- Het baanbrekende werk van Martin Fowler over Refactoring: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- De gids van Microsoft over Refactoring in Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/nl-nl/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Een gedetailleerd kijkje in Refactoring patronen met voorbeelden: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
