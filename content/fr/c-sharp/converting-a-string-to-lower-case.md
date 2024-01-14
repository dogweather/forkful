---
title:                "C#: Convertir une chaîne en minuscules."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi?

Lorsqu'on travaille avec des chaînes de caractères en C#, il est souvent nécessaire de les convertir en minuscules dans le but de les comparer ou de les traiter de manière uniforme. La conversion en minuscules est également utile pour s'assurer que les entrées utilisateur soient valides et cohérentes. Dans cet article, nous allons explorer comment convertir une chaîne en minuscules en utilisant C#.

## Comment faire?

La première étape pour convertir une chaîne en minuscules en C# est d'utiliser la méthode de classe `ToLower()`. Cette méthode prend en paramètre une chaîne de caractères et retourne une copie de cette chaîne en minuscules. Voici un exemple de code:

```C#
string message = "HELLO WORLD";
string lowerCaseMessage = message.ToLower();
Console.WriteLine(lowerCaseMessage);
```

Lorsque vous exécutez ce code, la console affichera `hello world`, car la chaîne a été convertie en minuscules.

Mais que se passe-t-il si vous avez besoin de convertir une chaîne qui contient des caractères avec des accents ou d'autres caractères spéciaux? Dans ce cas, il est recommandé d'utiliser la méthode `ToLowerInvariant()`. Cette méthode effectue la conversion en minuscules tout en ignorant les règles de mises en forme définies par la culture de votre système d'exploitation. Cela garantit un résultat prévisible et cohérent, indépendamment de la localisation du système.

Voici un exemple de code utilisant `ToLowerInvariant()`:

```C#
string message = "ÉLÉPHANT";
string lowerCaseMessage = message.ToLowerInvariant();
Console.WriteLine(lowerCaseMessage);
```

Lorsque vous exécutez ce code, la console affichera `éléphant`, en conservant l'accents sur la lettre "é".

## Plongée plus profonde

Le fait de convertir une chaîne en minuscules en C# utilise les règles définies par votre système d'exploitation. Cela peut être utile lorsque votre application a besoin de prendre en compte les règles de mise en forme de la langue et de la culture de l'utilisateur. Cependant, cela peut causer des problèmes si vous avez besoin d'une conversion en minuscules constante et prévisible, comme pour les tests unitaires. Dans ce cas, il est recommandé d'utiliser la méthode `ToLowerInvariant()` qui utilise simplement les règles ASCII pour la conversion en minuscules.

## Voir aussi

- [Méthode `ToLower()` en C# (Microsoft Docs)](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Méthode `ToLowerInvariant()` en C# (Microsoft Docs)](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolowerinvariant?view=netcore-3.1)