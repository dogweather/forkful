---
title:    "C#: Créer des nombres aléatoires"
keywords: ["C#"]
---

{{< edit_this_page >}}

Pourquoi: Générer des nombres aléatoires est un aspect important de la programmation, car cela permet de simuler des situations aléatoires et de rendre les programmes plus dynamiques.

Comment faire: Il existe plusieurs façons de générer des nombres aléatoires en C#, mais la méthode la plus simple consiste à utiliser la fonction "Random". Voici un exemple de code en C# pour générer un nombre aléatoire entre 1 et 10:

```C#
Random rand = new Random();
int randomNumber = rand.Next(1, 11);
Console.WriteLine(randomNumber);
```

Ce code crée un objet de type Random, puis utilise la méthode "Next" pour générer un nombre aléatoire entre 1 et 10. Ce nombre est ensuite imprimé à l'écran grâce à la fonction "WriteLine" de la console. Vous pouvez également utiliser cette méthode pour générer des nombres aléatoires entre une plage plus large de valeurs.

Plongée profonde: La méthode "Next" de la classe Random utilise un algorithme pour générer des nombres aléatoires à partir d'une "graine" initiale. Si vous ne spécifiez pas de graine, celle-ci sera générée à partir de l'horloge système de votre ordinateur. Cela signifie que si vous utilisez cette méthode à des moments rapprochés, vous risquez d'obtenir des nombres très similaires. Pour éviter cela, vous pouvez spécifier votre propre graine en utilisant la méthode "NextSeed" et ainsi obtenir des résultats plus variés.

Voir aussi: 

- [Documentation de la classe Random en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=net-5.0)
- [Générer des nombres aléatoires avec des limites personnalisées en C#](https://www.c-sharpcorner.com/article/random-number-generation-in-C-Sharp/)
- [Pourquoi et comment utiliser les nombres aléatoires en programmation](https://medium.com/swlh/why-and-how-to-use-random-numbers-in-programming-344f52167d9f)