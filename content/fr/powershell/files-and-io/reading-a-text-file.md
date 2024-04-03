---
date: 2024-01-20 17:55:02.542082-07:00
description: "Lire un fichier texte, c'est r\xE9cup\xE9rer son contenu pour l'utiliser\
  \ ou le manipuler. Les programmeurs font \xE7a pour traiter des donn\xE9es, configurer\
  \ des\u2026"
lastmod: '2024-03-13T22:44:58.076726-06:00'
model: gpt-4-1106-preview
summary: "Lire un fichier texte, c'est r\xE9cup\xE9rer son contenu pour l'utiliser\
  \ ou le manipuler."
title: Lecture d'un fichier texte
weight: 22
---

## What & Why?
Lire un fichier texte, c'est récupérer son contenu pour l'utiliser ou le manipuler. Les programmeurs font ça pour traiter des données, configurer des applications ou analyser des logs.

## How to:
Voici comment lire un fichier texte en PowerShell. Simple et rapide.

```PowerShell
# Lire tout le contenu d'un coup
$contenuComplet = Get-Content -Path "chemin/vers/ton/fichier.txt"
Write-Output $contenuComplet

# Lire ligne par ligne
Get-Content -Path "chemin/vers/ton/fichier.txt" | ForEach-Object {
    Write-Output $_
}
```
Résultat:
```
Voici la première ligne de votre fichier.
Et voici la deuxième ligne.
```

## Deep Dive
PowerShell est sorti en 2006, appelé à l'origine Windows PowerShell. Lire des fichiers est basique mais essentiel. `Get-Content` est la cmdlet standard pour ça, et elle vient avec des fonctionnalités pratiques comme le streaming de lignes avec des pipes (`|`) permettant de manipuler les données ligne par ligne sans surcharger la mémoire.

Alternative? Tu pourrais utiliser `[System.IO.File]::ReadAllText()` pour des opérations plus spécifiques ou de performance. Les détails comme la gestion de l'encodage des caractères ou le traitement asynchrone comptent dans des cas particuliers.

## See Also
- [Get-Content documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
- [Working with .NET classes](https://docs.microsoft.com/en-us/powershell/scripting/developer/hosting/adding-and-invoking-commands?view=powershell-7.1)
