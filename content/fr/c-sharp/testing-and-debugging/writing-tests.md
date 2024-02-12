---
title:                "Rédaction de tests"
date:                  2024-02-03T19:30:37.525125-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests en C# implique de créer des scripts automatisés pour valider la fonctionnalité de votre code, s'assurant qu'il se comporte comme prévu. Les programmeurs le font pour attraper les bugs tôt, faciliter le refactoring du code, et s'assurer que les nouvelles modifications ne cassent pas les fonctions existantes, augmentant ainsi la qualité et la fiabilité du logiciel.

## Comment faire :

Les développeurs C# utilisent principalement les cadres NUnit ou xUnit pour écrire des tests en raison de leur flexibilité et de leur ensemble de fonctionnalités étendu. Voici un exemple basique utilisant NUnit pour tester une simple fonction d'addition :

1. **Installez NUnit et NUnit3TestAdapter** via le Gestionnaire de Paquets NuGet ou le .NET CLI :
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Créez un projet de bibliothèque de classes C#** si vous ne l'avez pas déjà fait.

3. **Écrivez une fonction simple** à tester. Par exemple, une méthode d'addition dans une classe nommée `Calculator` :
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Écrivez une classe de test** en utilisant NUnit :
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Organise
            var calculator = new Calculator();
            int expected = 5;

            // Agit
            int actual = calculator.Add(2, 3);

            // Vérifie
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Exécutez le test** en utilisant l’exécuteur de test de votre IDE ou le .NET CLI :
```powershell
dotnet test
```

### Exemple de Sortie :

Supposant que votre test soit réussi, vous devriez voir une sortie similaire à cela :
```
Exécution du Test Réussie.
Tests totaux : 1
     Passés : 1
 Temps total : 1.2345 Secondes
```

### Utilisant xUnit :

Si vous préférez xUnit, la configuration est similaire à NUnit. Voici comment vous réécririez l'exemple de test pour la classe `Calculator` en utilisant xUnit :

1. **Installez xUnit et xUnit.runner.visualstudio** :
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Écrivez une classe de test en utilisant xUnit** :
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Organise
            var calculator = new Calculator();
            int expected = 5;

            // Agit
            int actual = calculator.Add(2, 3);

            // Vérifie
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Exécutez le test en utilisant le .NET CLI** ou l’exécuteur de test intégré de votre IDE.

NUnit et xUnit fournissent des fonctionnalités puissantes pour les tests paramétrés, les opérations de configuration/nettoyage, et l'organisation des tests en catégories, rendant ces outils indispensables dans la trousse à outils du programmeur C# pour assurer la qualité et la fonctionnalité du code.
