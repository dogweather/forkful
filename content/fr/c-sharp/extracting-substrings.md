---
title:                "Extraction de sous-chaînes"
html_title:           "C#: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?

L'extraction de sous-chaînes est une pratique courante dans la programmation en C#. Cela consiste à extraire une partie spécifique d'une chaîne de caractères, généralement en fonction d'un motif ou d'un index déterminé. Les programmeurs utilisent cette technique pour manipuler des données ou pour obtenir des informations précises à partir de grandes chaînes de texte.

## Comment faire :

Voici deux façons de procéder pour extraire une sous-chaîne en C# :

```C#
string chaine = "Bonjour tout le monde !";

// Méthode 1 : en utilisant la méthode Substring
string sousChaine = chaine.Substring(8, 4);
Console.WriteLine(sousChaine);
// Output : tout

// Méthode 2 : en utilisant l'opérateur de sélection []
string autreSousChaine = chaine[8..12];
Console.WriteLine(autreSousChaine);
// Output : tout
```

Dans ces exemples, nous extrayons les quatre caractères à partir du huitième index de la chaîne initiale. Les deux méthodes produisent le même résultat, mais la méthode Substring offre une plus grande flexibilité pour spécifier un motif ou une longueur de sous-chaîne, tandis que l'opérateur de sélection [] nécessite une manipulation manuelle de l'index.

## Plongée profonde :

L'extraction de sous-chaînes est une pratique courante depuis les débuts de la programmation, car elle permet de manipuler et de traiter des données plus facilement et efficacement. En C#, il existe également d'autres options pour extraire des sous-chaînes, telles que l'utilisation des expressions régulières ou de la classe Regex.

En ce qui concerne les performances, utiliser l'opérateur de sélection [] est généralement plus efficace que d'utiliser la méthode Substring, mais cela peut varier en fonction de la complexité du motif ou de la longueur de la chaîne initiale.

## Voir aussi :

Pour en savoir plus sur l'extraction de sous-chaînes en C# :

- [Documentation Microsoft sur la méthode Substring](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring)
- [Documentation Microsoft sur l'opérateur de sélection](https://docs.microsoft.com/fr-fr/dotnet/csharp/language-reference/proposals/csharp-8.0/range)
- [Documentation Microsoft sur les expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference)