---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:58:01.385432-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Vérifier l'existence d'un répertoire, c’est s’assurer qu’un chemin spécifique est valide et accessible. Les programmeurs font ça pour éviter les erreurs lors de la manipulation de fichiers ou de répertoires – pour que tout se déroule sans accroc.

## How to: (Comment faire :)
```PowerShell
# Vérifiez si le répertoire existe
$chemin = "C:\ExempleDossier"
if (Test-Path $chemin -PathType Container) {
    "Le répertoire existe."
} else {
    "Le répertoire n'existe pas."
}
```

Sortie échantillon si le répertoire existe :
```
Le répertoire existe.
```

Sortie échantillon si le répertoire n'existe pas :
```
Le répertoire n'existe pas.
```

## Deep Dive (Plongée en Profondeur)
Le cmdlet `Test-Path` est votre outil de prédilection pour vérifier l’existence des répertoires depuis PowerShell V1. Historiquement, les programmeurs effectuaient cette tâche en capturant des exceptions lors de tentatives d'accès ou de création. Heureusement, `Test-Path` est plus intuitif. Les alternatives ? Scripts .NET, ou lors de l'utilisation de PowerShell Core, l'API .NET `System.IO.Directory.Exists()`. La précision est clé : `Test-Path` a l'argument `-PathType` qui permet une distinction entre les fichiers et les répertoires (`Container` pour les répertoires).

## See Also (Voir Aussi)
- [about_Test-Path (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [about_if (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_if?view=powershell-7.1)
- [System.IO.Directory.Exists (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
