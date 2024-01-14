---
title:    "C#: Écriture de tests"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est souvent dit qu'il ne faut jamais sous-estimer l'importance des tests. Mais pourquoi est-il si important d'écrire des tests pour notre code ? La réponse est simple : les tests permettent de détecter les erreurs et les bugs et de s'assurer que notre code fonctionne correctement. Ils peuvent également servir de documentation pour notre code, en montrant comment celui-ci doit être utilisé.

## Comment faire

Il existe plusieurs façons de réaliser des tests en C#, mais la méthode la plus courante consiste à utiliser le framework de test intégré de Visual Studio. Voici un exemple de test simple qui vérifie si la méthode Add de notre classe Calculator renvoie le bon résultat :

```C#
[TestClass]
public class CalculatorTests
{
    [TestMethod]
    public void Add_ReturnsCorrectResult()
    {
        // Arrange
        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(2, 3);

        // Assert
        Assert.AreEqual(5, result);
    }
}
```

Nous créons d'abord une classe de test, avec l'attribut `[TestClass]`. À l'intérieur de celle-ci, nous définissons des méthodes de tests avec l'attribut `[TestMethod]`, qui servent à vérifier différents aspects de notre code. Dans cet exemple, nous vérifions si la méthode Add renvoie le bon résultat en utilisant la méthode `Assert.AreEqual`.

## Plongée en profondeur

Il est important de noter que les tests doivent être écrits avant même que le code ne soit écrit. En utilisant une approche de développement piloté par les tests (TDD), nous nous assurons que notre code est testé à chaque étape, évitant ainsi des erreurs potentielles à long terme.

Il existe également différents types de tests, tels que les tests unitaires, qui sont utilisés pour tester des parties spécifiques de notre code, et les tests d'intégration, qui visent à tester le bon fonctionnement de différentes parties du code ensemble.

Il est également important de garder à l'esprit qu'il est plus facile et moins coûteux de corriger des bugs détectés par des tests plutôt que par les utilisateurs finaux. Il est donc essentiel d'intégrer des tests dans notre processus de développement.

## Voir aussi

- [Documentation officielle Microsoft sur les tests en C#](https://docs.microsoft.com/fr-fr/dotnet/core/testing/)
- [Introduction au TDD avec C#](https://www.c-sharpcorner.com/article/introduction-to-test-driven-development-tdd-in-c-sharp/)
- [Guide du bon développeur : pourquoi écrire des tests ?](https://www.societe.com/guide-du-bon-developpeur-pourquoi-ecrire-des-tests-1937.html)