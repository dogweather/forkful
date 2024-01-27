---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests, c'est créer des scénarios automatisés pour vérifier que le code fait bien ce qu'il doit. Les développeurs le font pour prévenir les bugs, garantir le bon fonctionnement lors des modifications et s'assurer de la qualité.

## Comment faire :
On utilise souvent Xunit ou NUnit pour écrire des tests en C#. Voici un simple exemple avec Xunit :

```C#
using Xunit;

public class CalculatriceTests
{
    [Fact]
    public void TestAddition()
    {
        var calculatrice = new Calculatrice();
        Assert.Equal(5, calculatrice.Additionner(2, 3));
    }
}

public class Calculatrice
{
    public int Additionner(int a, int b)
    {
        return a + b;
    }
}
```

Compilez et exécutez les tests. Si tout est bon, le test passe :

```
Test Passed - TestAddition
```

## Deep Dive
Les tests unitaires sont devenus un standard dans les années 2000 avec l'essor des méthodologies Agile et TDD (Test-Driven Development). Il existe des frameworks alternatifs comme MSTest, mais Xunit et NUnit restent populaires pour leur simplicité et flexibilité. Pour un test efficace, il faut isoler les composants, utiliser des mocks quand nécessaire et écrire des cas de tests variés (positifs/négatifs).

## Voir aussi
- Documentation Xunit : [https://xunit.net/](https://xunit.net/)
- Guide NUnit : [https://nunit.org/](https://nunit.org/)
- TDD sur Microsoft Docs : [https://docs.microsoft.com/fr-fr/dotnet/core/testing/](https://docs.microsoft.com/fr-fr/dotnet/core/testing/)
