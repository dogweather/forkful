---
date: 2024-01-26 01:17:08.223293-07:00
description: "Comment faire : Refactorisons une m\xE9thode C# simple qui calcule et\
  \ imprime la somme d'un tableau de nombres : Avant le Remaniement ."
lastmod: '2024-04-05T22:38:58.337330-06:00'
model: gpt-4-0125-preview
summary: "Refactorisons une m\xE9thode C# simple qui calcule et imprime la somme d'un\
  \ tableau de nombres ."
title: Refactoring
weight: 19
---

## Comment faire :
Refactorisons une méthode C# simple qui calcule et imprime la somme d'un tableau de nombres :

Avant le Remaniement :
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
        Console.WriteLine("La somme est " + sum);
    }
}
```

Après le Remaniement :
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
        Console.WriteLine($"La somme est {CalculateSum()}");
    }
}

// Utilisation :
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

En remaniant, nous avons séparé les préoccupations, rendu la classe `Calculator` plus flexible en lui permettant de prendre n'importe quel tableau de nombres, et avons utilisé LINQ pour rendre le calcul de la somme plus concis.

## Approfondissement
Le remaniement trouve ses racines dans la communauté de programmation Smalltalk et a été popularisé dans les années 1990 par le livre de Martin Fowler "Refactoring: Improving the Design of Existing Code". Au fil des ans, il est devenu une partie fondamentale des méthodologies agiles et des bonnes pratiques de codage.

Il existe diverses approches du remaniement, telles que Red-Green-Refactor dans le Développement Piloté par les Tests (TDD). Cela garantit que le remaniement n'introduit pas de bugs en commençant par un test échoué, en le faisant réussir, puis en nettoyant le code.

Lors de la mise en œuvre du remaniement, il est crucial d'avoir une suite de tests complète pour garantir qu'aucune fonctionnalité ne soit rompue pendant le processus. Les outils de remaniement automatique, comme ReSharper pour C#, peuvent également aider dans ce processus en fournissant des moyens sûrs de modifier les structures de code. Cependant, les outils doivent être supplémentaires à une compréhension approfondie de la base de code et des principes de codage.

## Voir Aussi
- L'œuvre séminal de Martin Fowler sur le Remaniement : [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Le guide de Microsoft sur le Remaniement dans Visual Studio : [Remaniement (C#)](https://docs.microsoft.com/fr-fr/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Un regard détaillé sur les motifs de remaniement avec des exemples : [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
