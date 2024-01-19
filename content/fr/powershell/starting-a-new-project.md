---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Commencer un nouveau projet en programmation signifie créer un nouveau cadre de travail pour développer une nouvelle fonctionnalité ou une application. Les programmeurs le font pour organiser leur travail, faciliter la gestion des versions et optimiser la collaboration.

## Comment faire :

Pour commencer un nouveau projet en PowerShell, on utilise généralement des scripts. Voici un exemple de base pour créer un nouveau répertoire pour votre projet :

```PowerShell 
# créer un nouveau dossier appelé 'MonProjet'
New-Item -ItemType Directory -Path ./MonProjet
```

Pour se déplacer dans le nouveau répertoire :
```PowerShell 
Set-Location -Path ./MonProjet
```
Pour créer un nouveau fichier script :
```PowerShell 
New-Item -ItemType File -Name MonScript.ps1
```
Voila! Vous avez maintenant un nouveau projet avec un script PowerShell.

## Plongée profonde :

Historiquement, PowerShell a été développé par Microsoft pour automatiser les tâches administratives sur Windows. Il s'est ensuite étendu à d'autres plateformes et est maintenant largement utilisé pour la gestion de configuration et l'automatisation.

Il existe plusieurs alternatives pour commencer un nouveau projet, y compris Git et SVN, qui offrent des fonctionnalités supplémentaires comme le control de version et la collaboration à distance.

Concernant les détails d'implémentation, PowerShell utilise le .NET Common Language Runtime (CLR) et accepte les scripts écrits en .NET languages. Il permet une grande flexibilité et une puissance, permettant aux programmes de contrôler et d'automatiser presque toutes les aspects de Windows.

## Voir aussi :

Pour plus d'information sur PowerShell :
1. Site officiel PowerShell : https://docs.microsoft.com/fr-fr/powershell/
2. Tutorial PowerShell : https://www.tutorialspoint.com/powershell/index.htm
3. Créer un nouveau projet avec Git : https://docs.github.com/fr/github/getting-started-with-github/create-a-repo
4. Microsoft's guide on how to write a PowerShell script : https://docs.microsoft.com/fr-fr/powershell/scripting/learn/ps101/02-scripting-environment?view=powershell-7.1