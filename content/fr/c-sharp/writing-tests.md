---
title:                "C#: Ecriture de tests"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests dans votre code ?

Si vous êtes nouveau dans le monde de la programmation, vous vous demandez peut-être pourquoi écrire des tests est si important. La principale raison est que cela aide à identifier les erreurs dans votre code avant même qu'il ne soit exécuté. En écrivant des tests, vous pouvez vous assurer que votre code fonctionne correctement et vous économiserez du temps à le déboguer plus tard.

## Comment écrire des tests en C# ?

L'écriture de tests en C# est assez simple et peut être réalisée en utilisant la bibliothèque de test NUnit. Voici un exemple de code pour tester une fonction qui calcule la somme de deux nombres :

```
[C#]
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Should_ReturnCorrectSum()
    {
        // Arrange
        Calculator calculator = new Calculator();
        
        // Act
        int result = calculator.Add(2, 2);
        
        // Assert
        Assert.AreEqual(4, result);
    }
}
```

Dans cet exemple, nous créons une classe de test avec la bibliothèque NUnit et nous écrivons une méthode de test qui utilise la méthode "Add" de notre calculatrice pour vérifier si le résultat est correct. Vous pouvez écrire autant de tests que nécessaire pour couvrir toutes les fonctionnalités de votre code.

## Plongée en profondeur : Pourquoi écrire des tests ?

Écrire des tests pour votre code peut sembler fastidieux, mais cela présente de nombreux avantages. Tout d'abord, cela aide à détecter et à corriger les erreurs plus rapidement. Cela vous permet également de vérifier si de nouveaux changements dans votre code n'ont pas cassé des fonctionnalités précédemment développées. De plus, en créant des tests pour votre code, vous écrivez également une documentation sur son utilisation. Enfin, cela peut aider d'autres développeurs qui travaillent sur votre code à comprendre son fonctionnement plus rapidement.

## Voir aussi

- [Documentation NUnit](https://docs.nunit.org/)
- [Avantages de l'écriture de tests automatisés en C#](https://www.visualstudio.com/fr-gb/learn/test/why-test-home-vs.aspx)
- [Tutoriel de base pour écrire des tests en C#](https://docs.microsoft.com/fr-fr/dotnet/core/testing/)

**À propos de l'auteur :** [John Doe](https://monsite.com) est un développeur passionné par l'écriture de tests pour son code en C#. N'hésitez pas à le contacter pour toute question ou suggestion.