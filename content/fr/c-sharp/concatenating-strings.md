---
title:                "C#: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Le concaténation des chaînes de caractères est une opération importante dans de nombreux projets de programmation. Elle permet de combiner plusieurs chaînes de caractères pour en créer une nouvelle, souvent utilisée pour afficher du texte à l'écran ou pour construire des requêtes. Dans cet article, nous allons explorer comment concaténer des chaînes de caractères en utilisant le langage de programmation C#.

## Comment Faire

La concaténation de chaînes de caractères en C# est assez simple. Tout d'abord, nous devons créer deux ou plusieurs chaînes de caractères que nous voulons concaténer. Ensuite, nous pouvons utiliser l'opérateur "+" pour les combiner ensemble. Voyons un exemple concret dans le code ci-dessous :

```C#
// Déclaration de chaînes de caractères
string nom = "Jean";
string nomDeFamille = "Dupont";

// Concaténation avec l'opérateur "+"
string nomComplet = nom + " " + nomDeFamille;

// Affichage du résultat
Console.WriteLine(nomComplet); // Sortie : Jean Dupont
```

Dans cet exemple, nous avons créé deux chaînes de caractères contenant notre nom et notre nom de famille, puis nous les avons combinés avec l'opérateur "+". Le résultat de la concaténation est stocké dans une nouvelle variable appelée "nomComplet". Nous pouvons ensuite utiliser cette variable pour afficher notre nom complet à l'écran en utilisant la fonction "Console.WriteLine()".

Il est également possible d'utiliser la méthode "Concat()" de la classe "String" pour concaténer des chaînes de caractères. Cette méthode prend en paramètre deux chaînes de caractères et les concatène pour créer une nouvelle chaîne. Voici un autre exemple utilisant la méthode "Concat()":

```C#
// Déclaration de chaînes de caractères
string pays = "France";
string ville = "Paris";

// Appel à la méthode Concat()
string infos = string.Concat("Je suis actuellement à ", ville, ", en ", pays, ".");

// Affichage du résultat
Console.WriteLine(infos); // Sortie : Je suis actuellement à Paris, en France.
```

Remarquez comment nous avons utilisé la classe "string" pour appeler la méthode "Concat()" et concaténer les différentes parties de notre phrase.

## Deep Dive

En utilisant la concaténation de chaînes de caractères, il est important de faire attention aux types de données que nous utilisons. Par exemple, si nous essayons de concaténer une chaîne de caractères avec un nombre entier, cela peut entraîner une erreur car les types de données sont différents. Pour éviter cela, nous pouvons utiliser la méthode "ToString()" pour convertir nos nombres en chaînes de caractères avant de les concaténer.

De plus, il est important de noter que l'utilisation excessive de l'opérateur "+" pour concaténer de nombreuses chaînes de caractères peut avoir un impact sur les performances de notre application. Il est recommandé d'utiliser la classe "StringBuilder" lorsque vous avez besoin de concaténer un grand nombre de chaînes de caractères, car elle est plus efficace en termes de performances.

## Voir Aussi

Pour en savoir plus sur la concaténation de chaînes de caractères en C#, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Microsoft sur la concaténation de chaînes de caractères en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/how-to/concatenate-multiple-strings)
- [Tutoriel vidéo sur la concaténation de chaînes de caractères en C#](https://www.youtube.com/watch?v=xnOzGJLF5Jg)
- [Article de blog sur les performances de la classe StringBuilder en C#](https://exceptionnotfound.net/string-comparison-in-c-sharp-cheatsheet-for-benchmarking/)