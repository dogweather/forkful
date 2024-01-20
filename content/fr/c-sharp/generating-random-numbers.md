---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Générer des nombres aléatoires est un processus de création de chiffres qui ne peuvent pas être raisonnablement prédits, mieux que par le hasard. Les programmeurs générent des nombres aléatoires pour ajouter des éléments d'incertitude dans les jeux, les simulations et les tests de logiciels.

## Comment faire:
```C#
using System;

class Programme
{
    static void Main()
    {
        Random alea = new Random(); 
        int nombreAleatoire = alea.Next(1, 100); 
        Console.WriteLine(nombreAleatoire);
    }
}
```
Dans cet exemple, un nombre aléatoire entre 1 et 100 est généré et affiché à l'écran.

## Plongée Profonde
Historiquement, générer de véritables nombres aléatoires était compliqué à cause de la nature déterministe des ordinateurs. Maintenant, dans la plupart des langages de programmation, il y a des fonctions intégrées pour cela, comme `Random` en C#.

Il existe d'autres alternatives, comme l'algorithme de Mersenne Twister, qui est plus complexe mais aussi plus précis. De plus, pour un besoin de sécurité plus élevé, il est recommandé d'utiliser `RNGCryptoServiceProvider`.

L'implémentation détaillée de la classe `Random` en C# utilise un algorithme basé sur une équation linéaire pour générer des suites de nombres qui semblent aléatoires.

## Voir Aussi
Pour plus d'informations, consultez ces liens:
* [Random Number Generation (Wikipedia)](https://en.wikipedia.org/wiki/Random_number_generation)
* [Random Class (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
* [RNGCryptoServiceProvider Class (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)