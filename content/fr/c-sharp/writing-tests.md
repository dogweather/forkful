---
title:                "Ecrire des tests"
html_title:           "C#: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests peut sembler fastidieux pour certains, mais cela peut apporter de nombreux avantages à votre code. Parmi ceux-ci, on compte une meilleure fiabilité, une meilleure compréhension du code et une meilleure facilité pour détecter et corriger les erreurs.

## Comment faire

La première étape pour écrire un test en C# est d'ajouter la bibliothèque de tests appropriée à votre projet. Vous pouvez utiliser NUnit, xUnit ou encore Microsoft Visual Studio Testing Framework. Voici un exemple de code utilisant NUnit :

```
using NUnit.Framework;
using System;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_TwoNumbers_ReturnsCorrectResult()
    {
        // Arrange
        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(2, 3);

        // Assert
        Assert.AreEqual(5, result);
    }
}

public class Calculator
{
    public int Add(int num1, int num2)
    {
        return num1 + num2;
    }
}
```

Dans cet exemple, nous créons une classe de tests pour notre classe "Calculator". Nous utilisons la méthode "Add" de cette classe et vérifions si le résultat est bien égal à ce que nous attendons à l'aide de la méthode "Assert.AreEqual". Vous pouvez répéter ce processus pour toutes les méthodes que vous souhaitez tester.

## Deep Dive

Il existe différents types de tests que vous pouvez écrire en C#. Les tests unitaires, comme dans notre exemple précédent, vous permettent de tester une seule méthode ou fonction. Vous pouvez également écrire des tests d'intégration pour vérifier que différents composants de votre application fonctionnent ensemble correctement. Enfin, les tests de bout en bout (end-to-end) vous permettent de simuler une interaction avec l'utilisateur pour tester l'ensemble de votre application.

Il est également important de noter qu'écrire des tests vous forcera à avoir un code plus modulaire et facile à tester. Cela peut également améliorer la qualité globale de votre code.

## Voir aussi

- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [Microsoft Visual Studio Testing Framework](https://docs.microsoft.com/en-us/dotnet/core/testing/)