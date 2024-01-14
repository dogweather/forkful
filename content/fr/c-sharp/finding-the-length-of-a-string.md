---
title:                "C#: Trouver la longueur d'une chaîne de caractères"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Cela peut être utile pour vérifier si une entrée utilisateur est assez longue pour être valide, ou pour manipuler des données dans une base de données. Dans cet article, nous allons expliquer comment trouver la longueur d'une chaîne en utilisant le langage de programmation C#.

## Comment faire

La méthode la plus simple pour trouver la longueur d'une chaîne en C# est d'utiliser la propriété prenant en charge la méthode ```Length```. Par exemple, si nous avons une chaîne nommée ```nom```, nous pouvons utiliser ```nom.Length``` pour obtenir la longueur de la chaîne. Voici un exemple de code :

```C#
string nom = "John";
int longueur = nom.Length;
Console.WriteLine("La longueur du nom est : " + longueur);

// Output : La longueur du nom est : 4
```

Si nous voulons également inclure les espaces dans le calcul de la longueur, nous pouvons utiliser la méthode ```Trim()``` avant d'utiliser ```Length```. Cela supprimera tous les espaces inutiles de la chaîne avant de calculer sa longueur. Voici un exemple :

```C#
string nom = "   John   ";
int longueur = nom.Trim().Length;
Console.WriteLine("La longueur du nom est : " + longueur);

// Output : La longueur du nom est : 4
```

## Plongée en profondeur

En utilisant la méthode ```Length```, nous obtenons la longueur de la chaîne en comptant le nombre de caractères qu'elle contient, y compris les espaces. En revanche, si nous voulons compter le nombre de mots dans une chaîne, nous pouvons utiliser la méthode ```Split()``` et spécifier l'espace comme délimiteur. Ensuite, nous pouvons utiliser la propriété ```Length``` pour obtenir le nombre de mots. Voici un exemple :

```C#
string phrase = "Je suis un programmeur en herbe";
int nombreMots = phrase.Split(' ').Length;
Console.WriteLine("La phrase contient : " + nombreMots + " mots");

// Output : La phrase contient : 5 mots
```

## Voir aussi

- [Documentation de la méthode Length en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.length?view=netframework-4.8)
- [Documentation de la méthode Trim en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.trim?view=netframework-4.8)
- [Documentation de la méthode Split en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.split?view=netframework-4.8)