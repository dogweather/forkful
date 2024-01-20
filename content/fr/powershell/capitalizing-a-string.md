---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La capitalisation d'une chaîne signifie transformer les premières lettres de chaque mot en majuscules, pour des questions de présentation ou de formalité. Les programmeurs utilisent cette technique pour améliorer la lisibilité des titres, des noms propres ou des débuts de phrases.

## Comment faire :
```PowerShell
# Capitalisation simple d'une chaîne
$phrase = "bonjour le monde"
$phraseCapitalisee = $phrase | ForEach-Object { $_.Substring(0,1).ToUpper()+$_.Substring(1).ToLower() }
$phraseCapitalisee
# Résultat: Bonjour le monde

# Utilisation de la culture (française par exemple)
$cultureInfo = [System.Globalization.CultureInfo]::GetCultureInfo("fr-FR")
$phrase = "être ou ne pas être, telle est la question"
$phraseCapitalisee = (Get-Culture).TextInfo.ToTitleCase($phrase)
$phraseCapitalisee
# Résultat: Être Ou Ne Pas Être, Telle Est La Question
```

## Immersion
La capitalisation de chaîne existe depuis longtemps, dès lors que les systèmes ont dû traiter du texte. Dans le passé, elle pouvait être plus compliquée, car les langages de programmation offraient moins de fonctions intégrées. En PowerShell, avant que `[CultureInfo]` n'offre une gestion élégante des casse selon les cultures, il fallait écrire des fonctions manuelles pour capitaliser correctement selon les règles linguistiques. Aujourd'hui, il existe des alternatives comme `.ToTitleCase` pour respecter les différences culturelles, vital lors de la localisation des logiciels. L'implémentation sous-jacente utilise des tables de caractères et des règles de transformation pour chaque culture.

## À consulter aussi :
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [Microsoft's guide to CultureInfo class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)