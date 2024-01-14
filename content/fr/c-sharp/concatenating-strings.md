---
title:    "C#: Concaténer des chaînes de caractères"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est la concaténation de chaînes de caractères. Cette opération consiste à joindre plusieurs chaînes pour en former une seule. Mais pourquoi est-il important de savoir comment le faire en C# ? La réponse est simple : cela vous permettra de créer des messages personnalisés, des noms de fichiers dynamiques et de manipuler des données de manière efficace.

## Comment Faire

Dans cette section, nous allons voir comment utiliser la méthode `Concat()` pour concaténer des chaînes en C#. Tout d'abord, vous devez importer l'espace de noms `System` pour avoir accès aux classes et méthodes nécessaires.

```C#
Using System;

public class Program 
{
    public static void Main() 
    {
        // Définir deux chaînes à concaténer
        string nom = "Jean";
        string nomDeFamille = "Dupont";

        // Utiliser la méthode Concat pour joindre les deux chaînes
        string nomComplet = string.Concat(nom, nomDeFamille);

        // Afficher la chaîne résultante
        Console.WriteLine(nomComplet);
    }
}

// Output : JeanDupont
```

Il est également possible de concaténer plusieurs chaînes en une seule fois en passant un tableau de chaînes à la méthode `Concat()`. Vous pouvez également utiliser l'opérateur `+` pour concaténer des chaînes.

```C#
// Exemple de concaténation avec un tableau de chaînes
string[] parties = {"Hello", " ", "World"};
string message = string.Concat(parties);

// Output : Hello World
```

Il est important de noter que la méthode `Concat()` ne modifie pas les chaînes originales, mais retourne une nouvelle chaîne en les combinant.

## Deep Dive

Maintenant que vous savez comment concaténer des chaînes en C#, il est important de comprendre comment cela fonctionne réellement en coulisses. Lorsque vous utilisez la méthode `Concat()`, une nouvelle instance de la classe `StringBuilder` est créée en interne. Cette classe permet de manipuler des chaînes de manière efficace en modifiant directement leur contenu, au lieu de créer de nouvelles instances à chaque opération.

De plus, lorsqu'une chaîne est concaténée à une autre, une troisième chaîne est créée pour stocker le résultat. Plus vous concaténez de chaînes, plus ce processus devient coûteux en termes de performances. Pour remédier à cela, la classe `StringBuilder` est optimisée pour minimiser le nombre de chaînes créées lors d'opérations de concaténation.

## Voir aussi

- [Documentation sur la méthode Concat()](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.concat?view=net-5.0)
- [Documentation sur la classe StringBuilder](https://docs.microsoft.com/fr-fr/dotnet/api/system.text.stringbuilder?view=net-5.0)