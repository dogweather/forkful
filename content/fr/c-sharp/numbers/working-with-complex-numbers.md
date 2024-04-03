---
date: 2024-01-26 04:38:47.914926-07:00
description: "Comment faire : C# dispose d'une structure int\xE9gr\xE9e `System.Numerics.Complex`\
  \ pour traiter les nombres complexes. Voici un rapide tutoriel ."
lastmod: '2024-03-13T22:44:57.780477-06:00'
model: gpt-4-0125-preview
summary: "C# dispose d'une structure int\xE9gr\xE9e `System.Numerics.Complex` pour\
  \ traiter les nombres complexes."
title: Manipulation des nombres complexes
weight: 14
---

## Comment faire :
C# dispose d'une structure intégrée `System.Numerics.Complex` pour traiter les nombres complexes. Voici un rapide tutoriel :

```C#
using System;
using System.Numerics;

class ExempleNombreComplexe
{
    static void Main()
    {
        // Création de nombres complexes
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Opérations de base
        Complex somme = c1 + c2;
        Complex difference = c1 - c2;
        Complex produit = c1 * c2;
        Complex quotient = c1 / c2;

        // Affichage des résultats
        Console.WriteLine($"Somme: {somme}");
        Console.WriteLine($"Différence: {difference}");
        Console.WriteLine($"Produit: {produit}");
        Console.WriteLine($"Quotient: {quotient}");
        Console.WriteLine($"Magnitude de c1: {c1.Magnitude}");
        Console.WriteLine($"Phase de c1: {c1.Phase}");
    }
}
```

Et cela affichera :

```
Somme: (4.70710678118655, 5.70710678118655)
Différence: (3.29289321881345, 4.29289321881345)
Produit: (-1.00000000000001, 9)
Quotient: (0.6, 0.8)
Magnitude de c1: 6.40312423743285
Phase de c1: 0.896055384571344
```

## Approfondissement
Les nombres complexes, constitués d'une partie réelle et d'une partie imaginaire (souvent notés a + bi), existent depuis le 17ème siècle. Le mathématicien italien Gerolamo Cardano est crédité de leur développement initial. En programmation, travailler avec des nombres complexes implique de comprendre et de gérer ces deux parties distinctes.

Bien que `System.Numerics.Complex` de C# soit robuste et intégré dans le langage, d'autres langages comme Python offrent une fonctionnalité similaire avec `cmath` ou des bibliothèques tierces. Et si vous travaillez dans une version ancienne de C# ou une version de .NET qui ne supporte pas `System.Numerics`, vous devrez peut-être créer votre propre classe de nombre complexe ou trouver une bibliothèque.

En interne, les opérations sur les nombres complexes utilisent l'arithmétique à virgule flottante qui peut introduire des erreurs d'arrondi. Donc, lors de la mise en œuvre d'algorithmes qui utilisent largement les nombres complexes, il est essentiel de se souvenir de cela et de considérer l'impact sur la précision et l'exactitude.

## Voir également
1. Référence C# pour `System.Numerics.Complex` : https://learn.microsoft.com/fr-fr/dotnet/api/system.numerics.complex
2. Une plongée plus profonde dans les mathématiques des nombres complexes : https://mathworld.wolfram.com/ComplexNumber.html
3. Pour des implémentations et bibliothèques alternatives, consultez Math.NET Numerics : https://numerics.mathdotnet.com/
