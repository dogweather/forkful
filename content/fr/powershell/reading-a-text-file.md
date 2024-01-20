---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi ?

La lecture d'un fichier texte consiste à récupérer et à interpréter les informations stockées dans un fichier sous forme de texte. Les programmeurs la pratiquent pour manipuler ces informations à des fins diverses comme le traitement ou l'analyse de données.

## Comment faire :

```PowerShell
# Lire un fichier texte avec Get-Content
$get_content = Get-Content -Path .\monFichier.txt
Write-Output $get_content
```

Exemple de sortie:

```
Bonjour, comment ça va ?
Ceci est un fichier texte.
```

```PowerShell
# Lire une ligne spécifique avec Get-Content
$get_line = Get-Content -Path .\monFichier.txt -TotalCount 1
Write-Output $get_line
```

Exemple de sortie:

```
Bonjour, comment ça va ?
```

## Plongée en profondeur:

PowerShell a été lancé en 2006 et, depuis lors, a fourni une interface scriptable puissante pour les systèmes Windows. La lecture de fichiers texte était une partie essentielle de ses fonctionnalités dès le départ.

Pour des alternatives, .NET offre `StreamReader` et `File.ReadAllLines`. Ces alternatives ont leur propre utilité selon les ressources disponibles et les spécifications des tâches.

PowerShell utilise .NET en arrière-plan pour lire les fichiers texte. Lorsque vous utilisez `Get-Content`, PowerShell crée en réalité un `StreamReader`, lit le fichier ligne par ligne, puis ferme le StreamReader lorsque la fin de fichier est atteinte.

## Voir Aussi :

Pour plus d'informations, vous pouvez consulter les liens suivants:

1. [PowerShell Documentation](https://docs.microsoft.com/fr-fr/powershell/)
2. [Reading Files in .NET](https://docs.microsoft.com/fr-fr/dotnet/standard/io/how-to-read-text-from-a-file)
3. [About Functions in PowerShell](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_functions?view=powershell-7.1)