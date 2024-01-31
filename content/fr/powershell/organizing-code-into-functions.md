---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:11:41.458153-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Organiser le code en fonctions consiste à envelopper des blocs de code qui accomplissent des tâches spécifiques et à leur attribuer un nom. Cela est fait pour rendre le code réutilisable, lisible et maintenable. Au lieu de réécrire le même code, appelez une fonction. Vous souhaitez dépanner ou mettre à niveau ? Ajustez la fonction sans avoir à fouiller dans des tas de scripts.

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
