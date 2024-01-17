---
title:                "Extraction de sous-chaînes."
html_title:           "PowerShell: Extraction de sous-chaînes."
simple_title:         "Extraction de sous-chaînes."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
L'extraction de sous-chaînes consiste à récupérer un morceau spécifique d'une chaîne de caractères, en utilisant sa position par rapport à d'autres caractères. Les programmeurs le font généralement pour obtenir des données précises d'une chaîne plus grande, ou pour travailler avec des chaînes de caractères plus facilement manipulables.

## Comment faire:
Voici un exemple simple pour montrer comment extraire une sous-chaîne à partir d'une chaîne de caractères:
```PowerShell
$maChaine = "Bienvenue sur mon blog !"
$sousChaine = $maChaine.Substring(10, 11)
$sousChaine
```
Output:
```
mon blog
```

Le premier nombre dans la fonction `Substring` représente l'index de départ de la sous-chaîne, et le deuxième nombre représente la longueur de la sous-chaîne. Nous commençons à compter à partir de zéro, donc en utilisant 10 comme premier nombre, nous commençons à extraire la sous-chaîne à partir de la 11ème position dans la chaîne (espace compris).

## Plongée en profondeur:
L'extraction de sous-chaînes existe depuis de nombreuses années et a été introduite dans de nombreux langages de programmation, y compris PowerShell. Une alternative à la méthode `Substring` est la fonction `Split`, qui divise une chaîne en utilisant un délimiteur spécifique et retourne un tableau de sous-chaînes.

Il existe également d'autres méthodes pour extraire des sous-chaînes, telles que l'utilisation d'expressions régulières ou de méthodes spécifiques pour les chaînes de caractères, comme `Trim`, `Split` ou `Replace`.

L'implémentation de l'extraction de sous-chaînes dans PowerShell est basée sur la méthode `Substring` de la classe .NET `System.String`, ce qui signifie qu'elle fonctionne de la même manière que dans d'autres langages de programmation basés sur .NET.

## Voir aussi:
- [Documentation de Microsoft sur la méthode Substring](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring)
- [Autres méthodes pour extraire des sous-chaînes en PowerShell](https://www.tutorialspoint.com/powershell/powershell_substrings.htm)
- [Utilisation des expressions régulières en PowerShell](https://devblogs.microsoft.com/scripting/learn-to-use-regular-expressions-in-powershell-part-1/)