---
title:                "C#: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes, également connue sous le nom de substring en anglais, est une opération courante dans la programmation. Elle permet de sélectionner une partie spécifique d'une chaîne de caractères, en se basant sur un indice de début et un indice de fin. Cette fonctionnalité peut être très utile dans différentes situations de programmation, notamment pour manipuler et traiter des données.

## Comment faire

Pour effectuer une extraction de sous-chaîne en C#, nous pouvons utiliser la méthode `Substring()` qui est disponible dans la classe `string`. Cette méthode prend deux paramètres en entrée : l'indice de début et la longueur de la sous-chaîne désirée.

Voici un exemple de code C# utilisant la méthode `Substring()` :

```C#
string phrase = "Bonjour tout le monde!";
string sousChaine = phrase.Substring(8, 4);
Console.WriteLine(sousChaine);
```

Dans cet exemple, nous avons spécifié un indice de début de 8, ce qui correspond au premier caractère de la sous-chaîne souhaitée, ainsi qu'une longueur de 4 caractères. La console affichera donc le mot "tout" en tant que sous-chaîne extraite.

Il est également possible d'extraire une sous-chaîne à partir d'un indice de début jusqu'à la fin de la chaîne en utilisant la méthode `Substring()` avec un seul paramètre :

```C#
string nom = "Dupont";
string prenom = nom.Substring(0, 2);
Console.WriteLine(prenom);
```

Dans cet exemple, nous avons spécifié un indice de début de 0, ce qui correspond au premier caractère du nom, ainsi qu'une longueur de 2 caractères. La console affichera donc les deux premières lettres du nom, soit "Du".

## Plongée en profondeur

Il est important de noter que la méthode `Substring()` ne modifie pas la chaîne de caractères originale, elle retourne plutôt une nouvelle chaîne avec la sous-chaîne extraite. De plus, si la longueur spécifiée dépasse la longueur de la chaîne, une exception sera levée.

Il est également possible de spécifier un seul indice en tant que paramètre, ce qui indiquera à la méthode de commencer à cet indice et de retourner le reste de la chaîne.

```C#
string phrase = "Hello world!";
string sousChaine = phrase.Substring(6);
Console.WriteLine(sousChaine);
```

Dans cet exemple, la console affichera "world!", car nous avons spécifié l'indice de début 6, ce qui correspond au premier caractère après l'espace dans la chaîne "Hello world!".

## Voir aussi

Voici quelques ressources supplémentaires pour en apprendre davantage sur les extractions de sous-chaînes en C# :

- [Documentation officielle de Microsoft pour la méthode `Substring()`](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring)
- [Tutoriel sur les extractions de sous-chaînes en C#](https://www.tutorialspoint.com/csharp/csharp_substring.htm)
- [Vidéo explicative sur les extractions de sous-chaînes en C#](https://www.youtube.com/watch?v=w14tYhdP92Y)