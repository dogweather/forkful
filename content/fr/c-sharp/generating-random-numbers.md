---
title:    "C#: Génération de nombres aléatoires"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

#

Pourquoi: Les nombres aléatoires sont un élément essentiel de la programmation. Ils sont utilisés pour diverses tâches telles que la sélection aléatoire d'éléments dans une liste ou la génération de données aléatoires pour des simulations.

Comment faire: La génération de nombres aléatoires en C# est simple avec l'utilisation de la classe Random. Il suffit de l'instancier et d'appeler sa méthode Next() pour obtenir un nombre aléatoire. Voici un exemple de code :

```C#
Random generateur = new Random();
int nombreAleatoire = generateur.Next();
Console.WriteLine(nombreAleatoire);
```
La sortie de ce code sera un nombre entier aléatoire.

Pour générer des nombres aléatoires dans une plage spécifique, il suffit d'utiliser la méthode Next(min, max) en spécifiant les valeurs minimale et maximale souhaitées. Par exemple :

```C#
Random generateur = new Random();
int nombreAleatoire = generateur.Next(1, 10);
Console.WriteLine("Le nombre aléatoire est : " + nombreAleatoire);
```

La sortie de ce code sera un nombre entier aléatoire compris entre 1 et 10.

Plongée en profondeur: La classe Random utilise un algorithme pour générer des nombres aléatoires appelé algorithme congruentiel linéaire. Cet algorithme utilise une formule mathématique pour calculer les nombres aléatoires en fonction d'un nombre de départ appelé la "graine". Si vous utilisez la même graine, vous obtiendrez la même séquence de nombres aléatoires. Cela peut être utile pour déboguer votre code, mais il est recommandé de changer la graine à chaque exécution de votre programme pour obtenir une séquence différente de nombres aléatoires.

Voyons maintenant un exemple complet qui utilise la classe Random pour générer un nombre aléatoire qui correspond à un élément dans une liste :

```C#
Random generateur = new Random();
List<string> villes = new List<string> { "Paris", "Lyon", "Marseille", "Bordeaux", "Toulouse" };
int indexAleatoire = generateur.Next(0, villes.Count);
Console.WriteLine("La ville choisie au hasard est : " + villes[indexAleatoire]);
```

La sortie de ce code sera une ville choisie au hasard parmi la liste.

Vous pouvez également utiliser la méthode NextDouble() pour générer un nombre aléatoire à virgule flottante compris entre 0 et 1.

Voir aussi:
- Documentation officielle Microsoft pour la classe Random en C# : https://docs.microsoft.com/fr-fr/dotnet/api/system.random?view=netcore-3.1
- Tutoriel sur la génération de nombres aléatoires en C#: https://www.tutorialspoint.com/csharp/csharp_random_numbers.htm
- Exemples pratiques de génération de nombres aléatoires en C# : https://www.c-sharpcorner.com/blogs/generating-random-numbers-in-c-sharp1