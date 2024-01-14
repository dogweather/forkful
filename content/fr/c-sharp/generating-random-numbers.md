---
title:                "C#: Génération de nombres aléatoires"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans de nombreuses applications et projets de programmation, la génération de nombres aléatoires est une tâche courante et importante. Que ce soit pour créer des jeux, des simulations ou pour des tests de logiciel, les nombres aléatoires sont un élément essentiel pour ajouter un élément de variété et de chaos à nos programmes. Dans cet article, nous allons découvrir comment générer des nombres aléatoires en C# et approfondir les concepts clés liés à cette tâche.

## Comment faire

La génération de nombres aléatoires en C# peut être réalisée en utilisant la classe `Random` de la bibliothèque standard. Voici un exemple de code montrant comment créer une instance de cette classe et générer un nombre aléatoire entre 1 et 10 :

```C#
// Importer la bibliothèque Random
using System;

// Créer une instance de la classe Random
Random rand = new Random();

// Générer un nombre aléatoire entre 1 et 10
int randomNumber = rand.Next(1, 11);

// Afficher le résultat
Console.WriteLine(randomNumber);
```

Si vous exécutez ce code plusieurs fois, vous remarquerez que chaque fois un nombre différent est généré. Cela est dû au fait que la méthode `Next` de la classe `Random` utilise la valeur du temps actuel pour générer des nombres pseudo-aléatoires. Si vous souhaitez obtenir la même séquence de nombres à chaque exécution, vous pouvez utiliser une graine (seed) en passant un entier en paramètre pour initialiser la classe `Random`.

## Approfondissement

Le concept de "graine" (seed) est important lors de la génération de nombres aléatoires en programmation. La graine est l'élément initial qui est utilisé pour générer une séquence de nombres pseudo-aléatoires. En utilisant la graine, vous pouvez obtenir la même séquence de nombres à chaque exécution de votre programme. Cela peut être utile pour le débogage ou pour reproduire un comportement spécifique.

Une autre chose à prendre en compte lors de la génération de nombres aléatoires en C# est la qualité des nombres générés. La classe `Random` utilise un algorithme simple pour générer des nombres pseudo-aléatoires, ce qui peut conduire à des séquences de nombres prévisibles. Pour une génération de nombres plus complexe et de meilleure qualité, il est recommandé d'utiliser la classe `RandomNumberGenerator` de l'espace de noms `System.Security.Cryptography`.

Enfin, il est important de noter que les nombres générés par la classe `Random` sont des valeurs à virgule flottante (double) entre 0 et 1. Si vous souhaitez générer des nombres entiers dans une plage spécifique, vous pouvez utiliser la méthode `Next` en spécifiant la valeur minimale et maximale pour le nombre généré.

## Voir aussi

- [Documentation officielle de la classe Random en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.random)
- [Article sur les nombres aléatoires en C#](https://www.tutorialspoint.com/csharp/csharp_random_numbers.htm)
- [Génération de nombres aléatoires cryptographiquement sûrs en C#](https://www.c-sharpcorner.com/article/generating-cryptographically-secure-random-numbers-in-C-Sharp)