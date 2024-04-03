---
date: 2024-01-20 17:53:25.946819-07:00
description: "L'impression de sorties de d\xE9bogage, c'est afficher les valeurs et\
  \ les \xE9tats de votre script pendant son ex\xE9cution. On fait \xE7a pour comprendre\
  \ ce qui se\u2026"
lastmod: '2024-03-13T22:44:58.051008-06:00'
model: gpt-4-1106-preview
summary: "L'impression de sorties de d\xE9bogage, c'est afficher les valeurs et les\
  \ \xE9tats de votre script pendant son ex\xE9cution."
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## Comment faire :
Imprimer des messages de débogage avec `Write-Host` :

```PowerShell
Write-Host "Début du script."
$variable = "Salut, PowerShell !"
Write-Host "Valeur de la variable : $variable"
```

Résultat:
```
Début du script.
Valeur de la variable : Salut, PowerShell !
```

Pour un contrôle plus précis, utilisez `Write-Debug` :

```PowerShell
$DebugPreference = "Continue"
Write-Debug "Message de débogage."
```

Résultat:
```
DEBUG: Message de débogage.
```

Utiliser `Write-Verbose` pour des informations supplémentaires :

```PowerShell
$VerbosePreference = "Continue"
Write-Verbose "Information détaillée."
```

Résultat:
```
VERBOSE: Information détaillée.
```

## Deep Dive
Historiquement, le débogage se faisait avec des impressions de texte pour suivre le flux d’exécution. Dans le PowerShell, `Write-Host` a souvent été utilisé, mais c'est limité. Les cmdlets `Write-Debug` et `Write-Verbose` offrent plus de flexibilité. Avec des préférences `DebugPreference` et `VerbosePreference` on contrôle quand ces messages sont affichés. Les alternatives, comme des outils de profilage ou des environnements de développement intégrés (IDE) avec débogueurs intégrés, fournissent des capacités de débogage avancées sans imprimer de sortie dans la console.

## Voir Aussi
- [About Write-Debug](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug)
- [About Write-Verbose](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-verbose)
- [About automatic variables (like $VerbosePreference)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
