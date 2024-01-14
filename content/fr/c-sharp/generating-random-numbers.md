---
title:                "C#: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires en C#?

De nos jours, les programmes informatiques doivent souvent être en mesure de prendre des décisions aléatoires. Par exemple, un jeu vidéo pourrait avoir besoin de générer des positions aléatoires pour les ennemis ou un programme de simulation pourrait avoir besoin de générer des données aléatoires pour tester différents scénarios. La génération de nombres aléatoires en C# est un moyen simple et efficace de répondre à ces besoins.

## Comment le faire:

Pour générer des nombres aléatoires en C#, nous allons utiliser la classe `Random`. Cette classe fournit différentes méthodes pour générer différents types de nombres aléatoires, tels que des entiers, des nombres à virgule flottante et même des chaînes de caractères aléatoires.

```C#
// Génère un entier aléatoire entre 0 et 100
int randomNumber = new Random().Next(0, 101);

// Génère un nombre à virgule flottante aléatoire entre 0 et 1
double randomDouble = new Random().NextDouble();

// Génère une chaîne de caractères aléatoire composée de 10 lettres
string randomString = new Random().NextString(10);
```

Le résultat de ces différentes méthodes peut varier à chaque exécution du programme, ce qui est idéal pour les besoins de génération de nombres aléatoires.

## Plongée en profondeur:

Lorsque vous utilisez la classe `Random` en C#, il est important de comprendre comment elle génère en fait des nombres aléatoires. En réalité, les algorithmes utilisés pour la génération de nombres aléatoires en informatique ne sont pas vraiment aléatoires, mais ils simulent des valeurs aléatoires en utilisant des "semences" ou des "graines" pour calculer les nombres. Cela signifie que si vous utilisez la même graine, vous obtiendrez toujours la même séquence de nombres aléatoires.

Pour cette raison, il est recommandé d'utiliser une graine différente à chaque fois que vous avez besoin de générer des nombres aléatoires. Vous pouvez également utiliser le générateur de nombre aléatoire cryptographique `RandomNumberGenerator` si vous avez besoin de nombres aléatoires plus sécurisés pour des applications telles que le chiffrement de données.

# Voir aussi:

- [Documentation Microsoft pour la classe `Random`](https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=netcore-3.1)
- [Guide de génération de nombres aléatoires cryptographiques en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.security.cryptography.randomnumbergenerator?view=netcore-3.1)