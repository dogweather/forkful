---
title:                "Conversion d'une chaîne de caractères en minuscules"
aliases:
- /fr/powershell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:04.181923-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi?
Convertir une chaîne en minuscules, c'est passer tous ses caractères en minuscules. Les développeurs font ça pour uniformiser les données, faciliter les comparaisons et la recherche sans se soucier de la casse.

## How to: / Comment faire :
```PowerShell
# Transformer une chaîne en minuscules
$chaine = "Bonjour, je suis en MAJUSCULES!"
$chaine.ToLower()

# Résultat
bonjour, je suis en majuscules!
```

## Deep Dive / Plongée Profonde
Historiquement, convertir une chaîne en minuscules est fondamental en informatique pour les manipulations de textes. PowerShell utilise la méthode `.ToLower()` héritée de .NET pour réaliser cette tâche. Certains langages utilisent des fonctions telles que `strtolower()` en PHP ou `lower()` en Python.

En PowerShell, la méthode `.ToLower()` est culturellement neutre par défaut, utilisant les conventions de minuscules invariantes. Mais vous pouvez passer une culture spécifique si besoin, par exemple `.ToLower('fr-FR')` pour la culture française.

Alternativement, PowerShell 7+ propose l'opérateur `-ceq` pour effectuer une comparaison de chaînes sans tenir compte de la casse, sans avoir à changer la casse des chaînes en premier lieu.

En termes d'implémentation, considerer les variations linguistiques peut être important; certaines langues ont des règles spécifiques pour la casse des caractères.

## See Also / Voir Aussi
- [Documentation officielle de PowerShell](https://docs.microsoft.com/powershell/)
- [CultureInfo Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)

N'oubliez pas de valider votre code dans plusieurs scénarios et cultures!
