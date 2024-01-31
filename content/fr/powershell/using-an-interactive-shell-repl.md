---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:16:33.749237-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La coquille interactive, ou Boucle de Lecture-Évaluation-Affichage (REPL), vous permet de taper des commandes PowerShell et d'obtenir un retour immédiat. Les programmeurs l’utilisent pour tester rapidement des extraits de code, pour déboguer, ou pour apprendre de nouvelles commandes sans avoir à écrire un script complet.

## Comment faire :
Lancez PowerShell et vous êtes dans la REPL. Essayez le Cmdlet `Get-Date` :

```PowerShell
PS > Get-Date
```

Vous devriez voir la date et l'heure actuelles s'afficher :

```PowerShell
Mercredi, 31 mars 2023 12:34:56
```

Maintenant, enchaînez les commandes. Essayons de trier les processus par utilisation de mémoire :

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Cela affiche les 5 premiers processus par taille de l'ensemble de travail (utilisation de la mémoire).

## Plongée Profonde
La REPL de PowerShell trouve ses racines dans le shell Unix et d’autres shells de langages dynamiques comme celui de Python. Il s’agit d’un environnement d’exécution de commandes interactif pour un seul utilisateur. Contrairement à un langage compilé où vous écrivez des applications entières puis compilez, un environnement REPL vous permet d'écrire et d'exécuter du code une ligne à la fois. PowerShell prend également en charge l'exécution de scripts pour des tâches plus importantes.

Les alternatives pour Windows incluent l'invite de commandes ou d'autres REPL spécifiques à des langages comme IPython. Dans le monde Unix/Linux, des shells comme bash ou zsh remplissent une fonction similaire.

L'implémentation de PowerShell utilise une application hôte pour exécuter le shell. Si PowerShell.exe sous Windows est le plus commun, d'autres comme l'Environment de Scripting Intégré (ISE) ou le terminal intégré de Visual Studio Code peuvent également servir d'hôte.

## Voir Aussi
- [À propos de PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow : PowerShell](https://stackoverflow.com/questions/tagged/powershell)
