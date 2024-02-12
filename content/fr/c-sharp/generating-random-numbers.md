---
title:                "Génération de nombres aléatoires"
aliases:
- fr/c-sharp/generating-random-numbers.md
date:                  2024-01-27T20:32:55.756952-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en C# implique la création de valeurs numériques imprévisibles dans une plage spécifiée. Les programmeurs utilisent ces méthodes pour implémenter des fonctionnalités telles que la cryptographie, les simulations et les jeux où l'imprévisibilité ou la simulation de l'aléatoire du monde réel est nécessaire.

## Comment faire :

La manière la plus courante de générer des nombres aléatoires en C# est d'utiliser la classe `System.Random`. Voici un exemple simple démontrant son utilisation :

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Génère un nombre entre 1 et 99
        Console.WriteLine($"Nombre aléatoire : {randomNumber}");
    }
}
```

Cela affichera un nombre aléatoire tel que :

```
Nombre aléatoire : 42
```

Pour générer un nombre à virgule flottante aléatoire entre 0,0 et 1,0, vous pouvez utiliser la méthode `NextDouble` :

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Double aléatoire : {randomDouble}");
```

Si vous travaillez sur une application sensible à la sécurité qui nécessite une aléatoire cryptographique, il est préférable d'utiliser la classe `RNGCryptoServiceProvider` trouvée dans `System.Security.Cryptography` :

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Crée un nombre aléatoire de 4 octets de long
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Nombre aléatoire cryptographiquement sécurisé : {value}");
    }
}
```

## Plongée profonde

La génération de nombres aléatoires en C# a évolué au fil des ans. Initialement, la classe `System.Random` était la référence pour générer des nombres pseudo-aléatoires. Elle est pseudo-aléatoire parce que, étant donné une valeur de graine spécifique, elle produira la même séquence de nombres, ce qui peut être utile pour le débogage ou la répétabilité des tests.

Bien que suffisante pour les besoins basiques, `System.Random` n'est pas sûre pour les threads et peut produire des résultats prévisibles, ce qui n'est pas approprié pour les applications dépendantes de la sécurité. Cette limitation a conduit à l'introduction du `RNGCryptoServiceProvider` pour l'aléatoire cryptographique, qui est plus sécurisé mais aussi plus gourmand en ressources.

Une alternative dans .NET Core et .NET 5+ est la classe `RandomNumberGenerator` dans `System.Security.Cryptography` pour générer des nombres aléatoires de manière sécurisée, destinée comme une option plus moderne et facile à utiliser par rapport au `RNGCryptoServiceProvider`.

Chaque méthode de génération de nombres aléatoires en C# a sa place en fonction des exigences de l'application. Pour la plupart des applications, `System.Random` suffit, mais pour celles qui nécessitent des nombres aléatoires sécurisés et imprévisibles, les classes cryptographiques fournissent une alternative robuste.
