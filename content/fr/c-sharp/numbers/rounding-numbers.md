---
title:                "Arrondir les nombres"
aliases:
- /fr/c-sharp/rounding-numbers.md
date:                  2024-01-26T03:43:36.676943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Arrondir les nombres signifie les ajuster à la valeur de place la plus proche—pensez à les simplifier. Les programmeurs arrondissent pour contrôler la précision, améliorer la performance ou lors de l'affichage de résultats conviviaux pour l'utilisateur—comme les prix qui n'ont pas besoin de trois décimales.

## Comment faire :
Voici le billet aller-retour pour arrondir les nombres en C# :

```csharp
using System;

public class ExemplesArrondis
{
    public static void Main()
    {
        double nombreOriginal = 123.4567;

        // Arrondir au nombre entier le plus proche
        double arrondi = Math.Round(nombreOriginal);
        Console.WriteLine(arrondi); // Sortie : 123

        // Spécifier le nombre de décimales
        double arrondiDeuxDecimales = Math.Round(nombreOriginal, 2);
        Console.WriteLine(arrondiDeuxDecimales); // Sortie : 123.46

        // Arrondir vers le haut indépendamment du chiffre suivant
        double arrondiVersLeHaut = Math.Ceiling(nombreOriginal);
        Console.WriteLine(arrondiVersLeHaut); // Sortie : 124

        // Arrondir vers le bas indépendamment du chiffre suivant
        double arrondiVersLeBas = Math.Floor(nombreOriginal);
        Console.WriteLine(arrondiVersLeBas); // Sortie : 123
    }
}
```

## Exploration approfondie
Autrefois, arrondir était un jeu d'enfant pour réduire les coûts de calcul. Chaque cycle comptait, et réduire les nombres économisait un temps précieux. Fast-forward jusqu'au C# moderne, et il s'agit de gérer les doubles et les décimales réputés pour leurs erreurs de précision et leurs caprices d'affichage.

Au-delà de `Math.Round`, `Math.Floor` et `Math.Ceiling`, l'énumération `MidpointRounding` nous permet de dicter le sort des chiffres malheureusement situés à mi-chemin—c'est le carrefour entre les règles bancaires et l'équité du terrain de jeu de "arrondir à la demi supérieure".

Pour les publics plus exigeants, comme les applications mathématiques ou financières sérieuses, nous avons `decimal` plutôt que `double`, réduisant le drame de l'arrondi en offrant une précision plus élevée—moins d'arrondis, moins de problèmes.

## Voir aussi
- [Documentation officielle de C# sur `Math.Round`](https://docs.microsoft.com/fr-fr/dotnet/api/system.math.round)
- [Stack Overflow : Quand dois-je utiliser Double au lieu de Decimal ?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754)](https://fr.wikipedia.org/wiki/IEEE_754)
