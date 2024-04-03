---
date: 2024-01-26 03:50:51.027779-07:00
description: "Comment faire : Dans PowerShell, vous pouvez d\xE9boguer des scripts\
  \ en utilisant l'Environnement de Scripting Int\xE9gr\xE9 de PowerShell (ISE) int\xE9\
  gr\xE9 ou Visual\u2026"
lastmod: '2024-03-13T22:44:58.053511-06:00'
model: gpt-4-0125-preview
summary: "Dans PowerShell, vous pouvez d\xE9boguer des scripts en utilisant l'Environnement\
  \ de Scripting Int\xE9gr\xE9 de PowerShell (ISE) int\xE9gr\xE9 ou Visual Studio\
  \ Code (VS Code) avec l'extension PowerShell."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Dans PowerShell, vous pouvez déboguer des scripts en utilisant l'Environnement de Scripting Intégré de PowerShell (ISE) intégré ou Visual Studio Code (VS Code) avec l'extension PowerShell. Voici comment utiliser les points d'arrêt dans les deux :

### PowerShell ISE :
```PowerShell
# Définir un point d'arrêt sur une ligne spécifique
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Exécutez votre script normalement
.\MyScript.ps1

# Quand le script atteint le point d'arrêt, vous pouvez inspecter les variables
$myVariable

# Continuer l'exécution
Continue
```

### Visual Studio Code :
```PowerShell
# Ouvrez votre script PowerShell dans VS Code.
# Cliquez à gauche du numéro de ligne pour définir un point d'arrêt.
# Commencez à déboguer en appuyant sur F5 ou en cliquant sur 'Démarrer le débogage'.

# VS Code arrêtera l'exécution au point d'arrêt.
# Utilisez le panneau de débogage pour observer les variables, inspecter la pile d'appels et contrôler le déroulement.
```

Le débogage dans les deux environnements vous permet de pas à pas entrant (F11), pas à pas par-dessus (F10), et pas à pas sortant (Maj+F11) pendant le débogage.

## Exploration Approfondie
Historiquement, le débogage dans PowerShell était un peu lourd ; il nécessitait beaucoup de lignes `Write-Host` pour afficher l'état des variables ou la méthode classique d'essai et erreur. Avec l'avènement de PowerShell ISE, et plus récemment, de VS Code avec ses riches fonctionnalités de débogage, le débogage dans PowerShell est devenu presque aussi intuitif que dans les langages de programmation complets.

Les alternatives aux outils de débogage natifs de PowerShell incluent des outils tiers comme PowerGUI ou l'utilisation d'IDE robustes comme Visual Studio avec un plugin PowerShell.

Lors de l'implémentation d'un débogueur, considérez la portée du script, surtout lors du travail avec des scripts source ou des modules. Les points d'arrêt peuvent être basés sur des conditions, des changements de variables, ou des lignes, permettant un contrôle précis pendant une session de débogage.

De plus, avec la transition vers PowerShell Core (PowerShell multiplateforme), le débogage s'est largement déplacé entre les mains de VS Code, qui offre une expérience cohérente sur différentes plateformes.

## Voir Aussi
Pour en savoir plus sur le débogage dans PowerShell :
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
