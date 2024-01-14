---
title:    "C#: Écriture de tests"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de parler de la façon d'écrire des tests en C#, il est important de comprendre pourquoi les tests sont si importants pour les développeurs. Les tests sont un moyen efficace de vérifier la fonctionnalité de notre code et de détecter les erreurs avant qu'elles ne deviennent un problème majeur. Ils nous aident également à maintenir notre code propre, organisé et facilement modifiable. En fin de compte, les tests nous donnent la confiance nécessaire pour lancer notre code en production en sachant qu'il fonctionne correctement.

## Comment faire

Maintenant que nous avons compris pourquoi les tests sont si importants, passons à la partie pratique : comment écrire des tests en C#. Tout d'abord, nous devons ajouter une référence à la bibliothèque de test MSTest dans notre projet. Ensuite, nous pouvons commencer à écrire nos tests en utilisant la syntaxe suivante :

```C#
[TestMethod]
public void TestMaFonction()
{
    // Arrange
    MaClasse maClasse = new MaClasse();

    // Act
    int resultat = maClasse.MaFonction();

    // Assert
    Assert.AreEqual(10, resultat);
}
```

Dans cet exemple, nous créons une classe de test et utilisons la méthode `Assert.AreEqual()` pour vérifier si le résultat de la méthode `MaFonction()` est égal à 10. Si le test échoue, cela signifie que quelque chose ne fonctionne pas correctement dans notre code et qu'il doit être corrigé.

Bien sûr, cela n'est qu'un exemple très basique de test. Vous pouvez également écrire des tests pour des scénarios plus complexes en utilisant d'autres méthodes de la classe `Assert` tels que `AreNotEqual()`, `IsTrue()`, `IsFalse()`, etc. Il est également recommandé de créer des classes de test séparées pour chaque classe que vous testez, afin de maintenir votre code organisé et facile à comprendre.

## Plongée en profondeur

Écrire des tests peut sembler fastidieux et prendre plus de temps au début de votre projet, mais cela en vaut vraiment la peine à long terme. Non seulement cela vous permettra de détecter et de corriger les erreurs plus rapidement, mais cela vous aidera également à gagner en confiance et en efficacité en tant que développeur.

De plus, les tests vous permettent de mieux comprendre votre propre code et de l'utiliser comme une sorte de documentation vivante. Vous pouvez également utiliser des outils tels que Microsoft's Code Coverage pour vous assurer que vous testez toutes les parties de votre code.

## Voir aussi

Voici quelques ressources supplémentaires pour vous aider à en savoir plus sur l'écriture de tests en C# :

- [Documentation officielle de MSTest](https://docs.microsoft.com/fr-fr/dotnet/core/testing/unit-testing-with-mstest)
- [Guide sur les bonnes pratiques pour l'écriture de tests avec C#](https://enterprisecraftsmanship.com/posts/best-practices-for-writing-c-unit-tests/)
- [Introduction aux tests unitaires en C#](https://www.codeproject.com/Articles/178541/Introduction-to-Unit-Testing-with-Csharp)