---
title:                "Génération de nombres aléatoires"
html_title:           "C#: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Générer des nombres aléatoires est un processus qui permet aux programmeurs de créer des valeurs aléatoires dans leurs programmes. Cela peut être utile dans des jeux ou des simulations où des éléments aléatoires sont nécessaires pour créer une expérience plus réaliste.

## Comment faire:

### Utilisation de la fonction Random:

La méthode la plus commune pour générer des nombres aléatoires en C# est d'utiliser la classe Random. Voici un exemple de code qui utilise cette méthode pour générer un nombre aléatoire entre 1 et 10:

```C#
Random rand = new Random();
int randomNumber = rand.Next(1, 11); //génère un nombre aléatoire entre 1 et 10
Console.WriteLine(randomNumber); //affiche le nombre généré
```

### Personnaliser les limites:

Vous pouvez également personnaliser les limites des nombres aléatoires en utilisant la méthode Next avec deux paramètres, l'un pour le début de la plage possible et l'autre pour la fin de la plage possible.

```C#
int minLimit = 50;
int maxLimit = 100;
int randomNumber = rand.Next(minLimit, maxLimit + 1); //génère un nombre entre 50 et 100
```

## Plongée en profondeur:

### Contexte historique:

Générer des nombres aléatoires a été un défi pour les programmeurs depuis les débuts de l'informatique. Les méthodes utilisées varient selon les langages de programmation, mais l'objectif reste le même: créer une véritable aléatoire.

### Alternatives:

Bien que la classe Random soit la méthode la plus courante en C#, il existe d'autres options telles qu'utiliser des algorithmes complexes ou même utiliser des sources externes comme un générateur de nombres basé sur les mouvements de la souris.

### Détails de mise en œuvre:

L'algorithme utilisé par la classe Random en C# est appelé "mersenne twister" et il génère des nombres avec une distribution uniforme. Il est important de noter que ces nombres ne sont pas vraiment aléatoires, mais ils imitent bien les caractéristiques de l'aléatoire.

## À voir aussi:

- Documentation officielle de la classe Random en C#: https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=net-5.0
- Autres méthodes pour générer des nombres aléatoires en C#: https://www.tutorialsteacher.com/csharp/generating-random-numbers-in-csharp