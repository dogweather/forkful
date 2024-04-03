---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:03.646255-07:00
description: "Dans PowerShell, v\xE9rifier si un r\xE9pertoire existe est une t\xE2\
  che courante qui aide les scripts \xE0 prendre des d\xE9cisions bas\xE9es sur la\
  \ structure du syst\xE8me\u2026"
lastmod: '2024-03-13T22:44:58.071566-06:00'
model: gpt-4-0125-preview
summary: "Dans PowerShell, v\xE9rifier si un r\xE9pertoire existe est une t\xE2che\
  \ courante qui aide les scripts \xE0 prendre des d\xE9cisions bas\xE9es sur la structure\
  \ du syst\xE8me de fichiers\u2014comme \xE9viter les erreurs en confirmant qu'un\
  \ r\xE9pertoire cible est en place avant de tenter de lire ou d'\xE9crire dedans."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
PowerShell offre une manière directe de vérifier la présence d'un répertoire en utilisant le cmdlet `Test-Path`. Ce cmdlet retourne une valeur Booléenne indiquant si le chemin spécifié existe. Voici comment vous pouvez l'utiliser :

```powershell
# Vérifier si un répertoire existe
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Le répertoire existe-t-il ? $directoryExists"
```

Exemple de sortie pour un répertoire qui existe :

```
Le répertoire existe-t-il ? Vrai
```

Et pour un répertoire qui n'existe pas :

```
Le répertoire existe-t-il ? Faux
```

Pour des scripts plus complexes, en particulier ceux interagissant avec des partages réseau ou un stockage cloud, vous pourriez avoir besoin de vérifications supplémentaires ou de fonctionnalités non directement disponibles via `Test-Path`. Dans de tels cas, utiliser des modules PowerShell tiers ou des bibliothèques peut être bénéfique, bien que la plupart des tâches routinières puissent être accomplies avec les cmdlets intégrés de PowerShell. Au moment de ma dernière mise à jour des connaissances, il n'y a pas eu de bibliothèque tierce largement adoptée spécifiquement pour vérifier l'existence d'un répertoire au-delà de ce que `Test-Path` offre, principalement parce que `Test-Path` lui-même est à la fois robuste et efficace dans cet objectif.
