---
date: 2024-01-26 04:16:33.749237-07:00
description: "La coquille interactive, ou Boucle de Lecture-\xC9valuation-Affichage\
  \ (REPL), vous permet de taper des commandes PowerShell et d'obtenir un retour imm\xE9\
  diat.\u2026"
lastmod: '2024-03-13T22:44:58.049417-06:00'
model: gpt-4-0125-preview
summary: "La coquille interactive, ou Boucle de Lecture-\xC9valuation-Affichage (REPL),\
  \ vous permet de taper des commandes PowerShell et d'obtenir un retour imm\xE9diat."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

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
