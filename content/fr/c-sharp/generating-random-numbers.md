---
title:                "Générer des nombres aléatoires"
html_title:           "C#: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur, vous avez sûrement été confronté à la nécessité de générer des nombres aléatoires dans votre code. Que ce soit pour simuler des jeux, des tests ou encore pour créer des données de manière aléatoire, les nombres aléatoires sont un outil essentiel dans le domaine de la programmation.

## Comment faire

La génération de nombres aléatoires en C# est très simple grâce à la classe Random. Tout d'abord, il faut déclarer un objet de type Random en utilisant l'instruction ```C# Random random = new Random(); ```. Ensuite, vous pouvez utiliser différentes méthodes de cette classe pour générer des nombres aléatoires selon vos besoins.

Pour générer un nombre entier aléatoire, vous pouvez utiliser la méthode ```C# Next()``` en précisant une valeur minimale et maximale à l'aide de paramètres. Par exemple, pour générer un nombre entre 1 et 100, vous pouvez utiliser ```C# random.Next(1, 101)```.

Si vous avez besoin de nombres décimaux aléatoires, vous pouvez utiliser la méthode ```C# NextDouble()```, qui retourne un nombre entre 0 et 1. Vous pouvez ensuite utiliser des opérations mathématiques pour ajuster la plage de valeurs selon vos besoins.

Enfin, pour générer des nombres aléatoires dans un ordre spécifique, vous pouvez utiliser la méthode ```C# NextBytes()```, qui remplira un tableau de bytes avec des valeurs aléatoires.

## Plongez plus profondément

La classe Random est basée sur un algorithme pseudo-aléatoire qui utilise une "graine" pour produire des nombres aléatoires. Par défaut, la graine est basée sur l'horloge du système, mais vous pouvez la définir manuellement en utilisant la méthode ```C# Random(seed)```. Cela peut être utile si vous avez besoin de générer la même séquence de nombres aléatoires plusieurs fois.

Il est également possible de générer des nombres aléatoires selon une distribution spécifique en utilisant les méthodes de la classe Math. Par exemple, la méthode ```C# Math.Round()``` peut être utilisée pour générer un nombre aléatoire entier selon une distribution normale.

## Voir aussi

- [La classe Random sur le site Microsoft Docs](https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=net-5.0)
- [Un tutoriel sur la génération de nombres aléatoires en C#](https://www.codeproject.com/Articles/1131528/Generating-Random-Numbers-in-Csharp-An-Elaborate-A)
- [Plus de détails sur les distributions aléatoires en C#](https://www.c-sharpcorner.com/UploadFile/mahesh/default-random-number-generator-in-C-Sharp/)