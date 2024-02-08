---
title:                "Lecture des arguments de ligne de commande"
aliases:
- fr/powershell/reading-command-line-arguments.md
date:                  2024-01-20T17:56:41.813707-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire des arguments de ligne de commande, c'est récupérer les données fournies à un script lors de son exécution. Les programmeurs le font pour personnaliser le comportement d'un script sans le modifier directement.

## How to:
PowerShell permet de jouer avec les arguments de commande de manière assez directe. Voici comment on s'y prend. 

```PowerShell
# Exemple d'un script PowerShell qui lit les arguments de ligne de commande.

# Pour exécuter ce script avec des arguments : .\script.ps1 arg1 arg2 arg3

# Afficher tous les arguments :
Write-Host "Tous les arguments: $args"

# Accéder au premier argument :
Write-Host "Premier argument: $($args[0])"

# Nombre d'arguments :
Write-Host "Nombre d'arguments: $($args.count)"
```

Et voici ce que vous pourriez voir s'afficher :

```
Tous les arguments: arg1 arg2 arg3
Premier argument: arg1
Nombre d'arguments: 3
```

## Deep Dive
Historiquement, lire des arguments de ligne de commande permettait aux premiers programmes d'avoir un degré d'interaction avec l'utilisateur. En PowerShell, `$args` est une variable automatique qui contient un tableau des arguments non traités. Alternativement, on peut employer les `param` pour définir des arguments plus formels avec des noms et types spécifiques.

Les paramètres peuvent être rendus obligatoires ou optionnels, et on peut même leur assigner des valeurs par défaut. Voici un exemple :

```PowerShell
param(
  [Parameter(Mandatory=$true)]
  [string]$FilePath,
  
  [Parameter(Mandatory=$false)]
  [int]$MaxLines = 10
)

Write-Host "Traitement du fichier: $FilePath avec un maximum de $MaxLines lignes."
```

Côté implémentation, les arguments de ligne de commande sont traditionnellement des chaînes de caractères, mais PowerShell permet leur conversion automatique en types plus spécifiques, améliorant ainsi la robustesse des scripts.

## See Also
- [about_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters)
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters)
- [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
