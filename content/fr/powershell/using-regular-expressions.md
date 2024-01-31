---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
simple_title:         "Utilisation des expressions régulières"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Les expressions régulières, ou regex, sont des motifs utilisés pour matcher des chaînes de caractères dans du texte. Les programmeurs les utilisent pour valider, rechercher ou manipuler des données facilement.

## How to:
```PowerShell
# Trouver un numéro de téléphone dans un texte
$texte = 'Mon numéro est 06-123-456 mais celui de Laura est 07-234-567'
$regex = '\b\d{2}-\d{3}-\d{3}\b'
[regex]::Matches($texte, $regex) | ForEach-Object { $_.Value }

# Sortie attendue:
# 06-123-456
# 07-234-567

# Remplacer des espaces multiples par un seul espace
$phrase = 'Ceci    est    un   test.'
$phrase -replace '\s+', ' '

# Sortie attendue:
# Ceci est un test.
```

## Deep Dive
Les regex existent depuis les années 1950, initialement théorisées par le mathématicien Stephen Kleene. Alternativement, on peut utiliser des méthodes de chaînes comme `.Contains()`, `.IndexOf()`, mais les regex offrent plus de flexibilité pour les motifs complexes. En PowerShell, les regex sont implémentées via le type `[regex]`, une classe du .NET Framework qui supporte des opérations performantes de correspondance de chaînes.

## See Also
- [Microsoft Official Documentation for Regular Expressions](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex101: Online regex tester and debugger](https://regex101.com/)
- [PowerShell Gallery Script Samples](https://www.powershellgallery.com/)
