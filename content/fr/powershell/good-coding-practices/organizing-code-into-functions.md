---
date: 2024-01-26 01:11:41.458153-07:00
description: "Organiser le code en fonctions consiste \xE0 envelopper des blocs de\
  \ code qui accomplissent des t\xE2ches sp\xE9cifiques et \xE0 leur attribuer un\
  \ nom. Cela est fait\u2026"
lastmod: '2024-03-13T22:44:58.054762-06:00'
model: gpt-4-1106-preview
summary: "Organiser le code en fonctions consiste \xE0 envelopper des blocs de code\
  \ qui accomplissent des t\xE2ches sp\xE9cifiques et \xE0 leur attribuer un nom."
title: Organisation du code en fonctions
weight: 18
---

## Comment faire :
Écrivons une fonction pour calculer la somme de deux nombres. C'est simple, mais cela illustre le principe.

```PowerShell
function Ajouter-Nombres {
    param (
        [int]$PremierNum,
        [int]$DeuxiemeNum
    )
    return $PremierNum + $DeuxiemeNum
}

# Appeler la fonction avec 5 et 10
$somme = Ajouter-Nombres -PremierNum 5 -DeuxiemeNum 10
Write-Output "La somme est $somme"
```

Résultat de l'exemple :

```
La somme est 15
```

## Plongée en profondeur
Les fonctions dans PowerShell, comme dans la plupart des langages, ne sont pas une nouveauté. Nous compartimentons le code depuis l’époque de Fortran. Il s'agit de 'ne pas réinventer la roue'. Des alternatives ? Certes, des scripts ou des cmdlets. Mais ils manquent de l'ordre et de la sensibilité au contexte des fonctions au sein des scripts.

Mise en œuvre ? Les fonctions peuvent être basiques comme notre exemple ou complexes avec des portées, une entrée de pipeline et plus encore. Prenons les `Fonctions Avancées`. Elles imitent les cmdlets avec des paramètres qui ont des attributs, comme `[Parameter(Mandatory=$true)]`. C'est un aperçu de la flexibilité de PowerShell.

## Voir aussi
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
