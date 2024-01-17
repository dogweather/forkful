---
title:                "Concaténation de chaînes"
html_title:           "C#: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La concaténation de chaînes, c'est lorsqu'un programmeur combine plusieurs chaînes de caractères en une seule. Cela peut être utile dans de nombreuses situations, comme pour créer des messages d'erreur personnalisés ou formater des données pour l'affichage.

## Comment faire:

Voici comment concaténer des chaînes de caractères en C#:

```C#
// Déclaration de deux chaînes à concaténer
string str1 = "Bonjour";
string str2 = "le monde!";

// Utilisation de l'opérateur "+" pour concaténer les deux chaînes
string strConcat = str1 + " " + str2;

// Affichage du résultat
Console.WriteLine(strConcat);
```

Le résultat sera:

```
Bonjour le monde!
```

## Plongez plus en profondeur:

La concaténation de chaînes en programmation n'est pas une pratique nouvelle. Elle est utilisée depuis les débuts de la programmation pour manipuler et formater des données textuelles. Cependant, dans certains cas, il est préférable d'utiliser des alternatives telles que la classe StringBuilder ou la méthode string.Format() pour améliorer les performances.

## Voir aussi:

- [Documentation Microsoft pour la concaténation de chaînes en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/how-to/concatenate-multiple-strings)
- [Utilisation de la classe StringBuilder pour concaténer efficacement des chaînes en C#](https://www.c-sharpcorner.com/article/string-concatenation-using-string-builder-in-c-sharp/)
- [Différences entre l'opérateur "+" et la méthode Concat pour la concaténation de chaînes en C#](https://www.infosihat.com/concatenation-of-string-in-c-sharp-concat-method-and-operator/)