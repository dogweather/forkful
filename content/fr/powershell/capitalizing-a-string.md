---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "PowerShell: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?

La capitalisation d'une chaîne de caractères est l'action de convertir toutes les premières lettres d'un texte en majuscules. Les programmeurs utilisent cette technique pour améliorer la lisibilité ou pour l'assortir à une norme de mise en forme spécifique.

## Comment faire:

Voici comment vous pouvez capitaliser une chaîne de caractères en PowerShell:

```PowerShell
$maChaine = "bonjour le monde"
$maChaineCapitalisée = (Get-Culture).TextInfo.ToTitleCase($maChaine.ToLower())
echo $maChaineCapitalisée
```

Résultat affiché:

```PowerShell
Bonjour Le Monde
```

## Approfondissement:

La capitalisation de chaînes est assez courante en programmation et a été facilitée dans les versions récentes de PowerShell. En termes d'alternatives, vous pouvez également utiliser la méthode `.ToUpper()` pour convertir la chaîne entière en majuscules, et non pas seulement la première lettre de chaque mot.

```PowerShell
$maChaine = "bonjour le monde"
$maChaineMajuscule = $maChaine.ToUpper()
echo $maChaineMajuscule
```

Résultat affiché:

```PowerShell
BONJOUR LE MONDE
```

Pour information, la méthode `(Get-Culture).TextInfo.ToTitleCase()` fonctionne en convertissant d'abord toute la chaîne en minuscules, puis en mettant chaque première lettre de chaque mot en majuscule.

## Voir aussi:

Pour d'autres ressources sur PowerShell, consultez les liens ci-dessous:

1. [_Documentation officielle de PowerShell_](https://docs.microsoft.com/fr-fr/powershell/)
2. [_PowerShell Basic Cheat Sheet_](https://www.withdave.com/2017/11/powershell-basic-cheat-sheet/)
3. [_String Manipulation with PowerShell_](https://adamtheautomator.com/string-manipulation-with-powershell/)