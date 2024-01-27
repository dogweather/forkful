---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Mettre une chaîne de caractères en majuscules, c'est transformer toutes ses lettres en majuscules. Les programmeurs utilisent cette manipulation pour normaliser les entrées de texte, pour de l'affichage ou des comparaisons de chaînes insensibles à la casse.

## How to:
En C#, la capitalisation se fait avec la méthode `ToUpper()` :

```C#
string original = "Bonjour tout le monde!";
string majuscule = original.ToUpper();

Console.WriteLine(majuscule);
// Sortie : BONJOUR TOUT LE MONDE!
```

## Deep Dive
La méthode `ToUpper()` existe depuis les premières versions de .NET Framework. Elle utilise les règles de culture (la culture par défaut étant `CultureInfo.CurrentCulture`) pour transformer les lettres en majuscules. 
Des alternatives incluent `ToUpperInvariant()`, qui ignore les règles culturelles locales pour une mise en majuscule standard (utile pour les protocoles et les formats de stockage).

Un point crucial de l'implémentation est de comprendre que C# est basé sur Unicode, ce qui signifie que `ToUpper()` gère non seulement l’alphabet anglais, mais aussi d'autres alphabets et symboles. Attention, certains caractères n'ont pas d'équivalent majuscule.

## See Also
- Documentation Microsoft sur `String.ToUpper`: https://docs.microsoft.com/fr-fr/dotnet/api/system.string.toupper
- Article sur l'utilisation de `CultureInfo`: https://docs.microsoft.com/fr-fr/dotnet/api/system.globalization.cultureinfo
- Discussion sur l'Unicode et C# : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/character-encoding
