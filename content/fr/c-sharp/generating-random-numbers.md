---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:34.882207-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Générer des nombres aléatoires, c'est comme lancer un dé virtuel. Les programmeurs les utilisent pour des jeux, des simulations ou des tests où l'imprévisibilité est essentielle.

## How to: (Comment faire :)
```C#
using System;

class Program
{
    static void Main()
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 101); // Génère un nombre entre 1 et 100
        Console.WriteLine(randomNumber); // Affiche le nombre
    }
}
```
Exemple de sortie :
```
42
```

## Deep Dive (Plongée en profondeur)

Historiquement, les nombres aléatoires en informatique ne sont pas vraiment "aléatoires". Ils sont souvent déterminés par des algorithmes prévisibles, d'où le terme "pseudo-aléatoire". En C#, `System.Random` est suffisant pour les besoins généraux non sécuritaires. Pour une sécurité accrue, depuis .NET Core, utilisez `System.Security.Cryptography.RandomNumberGenerator`. Ce dernier est conçu pour les scénarios cryptographiques où la prévisibilité est inacceptable.

Alternatives :
- `Guid.NewGuid().GetHashCode()` - pour un moyen rapide et sale sans grandes prétentions de qualité aléatoire.
- `RandomNumberGenerator.GetInt32()` - pour un remplacement direct et plus sécurisé du `Random.Next()`.

Implémentation :
Lors de l'utilisation de `System.Random`, si instancié plusieurs fois dans un court intervalle, cela peut conduire à des résultats identiques. Pour éviter cela, réutilisez l'instance de `Random` ou utilisez un semeur (`seed`) statique global.

## See Also (Voir également)

- Documentation Microsoft sur [System.Random](https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=net-6.0)
- Approfondissement sur [RandomNumberGenerator](https://docs.microsoft.com/fr-fr/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0)
- Concept des nombres pseudo-aléatoires sur [Wikipedia](https://fr.wikipedia.org/wiki/G%C3%A9n%C3%A9rateur_de_nombres_pseudo-al%C3%A9atoires)