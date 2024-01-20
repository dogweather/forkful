---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'interpolation de chaînes est une méthode où on insère des valeurs dans une chaîne littérale, d'où son autre appellation "chaîne formatée". Les programmeurs l'utilisent pour combiner des valeurs variables avec du texte de manière plus lisible et propre. 

## Comment faire :

Créons une chaîne interpolée simple dans PowerShell : 

```PowerShell
$nom = "Dupont"
$salutation = "Bonjour, $nom"
$salutation  # Résultats en "Bonjour, Dupont"
```
La variable `$nom` est insérée dans la chaîne littérale `$salutation`. 

Une utilisation plus compliquée pourrait ressembler à ceci :

```PowerShell
$age = 25
$message = "$nom a $age ans"
$message  # Résultats en "Dupont a 25 ans"
```
Ici, `$nom` et `$age` sont tous deux insérés dans la chaîne `$message`.

## Plongeon Profond

Historique : Avant Powershell 5, les programmeurs utilisaient la méthode de format pour interpoler les chaînes. Mais l'interpolation de chaîne est devenue plus courante en raison de son accessibilité et de sa lisibilité. 

Alternatives : On peut toujours utiliser la méthode de format, comme `$message = "{0} a {1} ans" -f $nom, $age`. 

Détails d'implémentation : L'opératrice d'interpolation est `$`. Les doubles guillemets (`"`) indiquent à PowerShell d'inspecter la chaîne à des fins d'interpolation, tandis que les guillemets simples (`'`) retourneraient la chaîne littérale.

## Voir Aussi 

- [Documentation de Microsoft sur les opérations de chaîne](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#string)

N'oubliez pas que l'interpolation des chaînes est un outil puissant pour rendre votre code plus lisible et facile à comprendre. Amusez-vous à coder !