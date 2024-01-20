---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La lecture des arguments de la ligne de commande est le processus par lequel un programme de script récupère des informations ou des paramètres passés lors de l'exécution. Les développeurs font cela pour une interaction utilisateur flexible et une exécution conditionnelle de leur code.

## Comment Faire :

Voici une illustration de base sur la façon de lire les arguments de la ligne de commande en PowerShell.

```PowerShell
# Sample Code
param (
  [string]$param1,
  [string]$param2
)

Write-Host "Paramètre 1: $param1"
Write-Host "Paramètre 2: $param2"
```

Si vous exécutez ce script avec des arguments de ligne de commande comme ci-dessous :

```PowerShell
.\myScript.ps1 -param1 "bonjour" -param2 "le monde"
```

Le résultat sera :

```PowerShell
Paramètre 1: bonjour
Paramètre 2: le monde
```

## Plongée Profonde :

Historiquement, la lecture des arguments de la ligne de commande a été une pratique commune dans les langages de programmation de script tels que bash et autres langages C.

Une alternative à cela est l'utilisation des variables d'environnement, mais elles sont généralement moins souples et requièrent plus d'effort pour la gestion des erreurs.

Lors de la lecture des arguments de la ligne de commande en PowerShell, le premier argument non attribué à une variable par un tiret (-) est considéré comme un argument positionnel. Ces arguments sont attribués aux variables déclarées dans la section Param() dans l'ordre où ils apparaissent.

## Voir Aussi :

Pour plus d'informations sur les techniques et les options avancées lors de la lecture des arguments de la ligne de commande dans PowerShell, consultez les liens suivants :

1. [Documentation officielle de PowerShell](https://docs.microsoft.com/fr-fr/powershell/)
2. [Guide PowerShell sur les arguments de la ligne de commande](https://ss64.com/ps/syntax-args.html)