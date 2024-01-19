---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Créer un Fichier Temporaire en PowerShell 

## Quoi et Pourquoi?
Créer un fichier temporaire consiste à générer un fichier à usage unique et à court terme pour des raisons spécifiques. Les programmeurs le font généralement pour le stockage temporaire des données, faciliter le debugging, ou pour manipuler des informations entre différentes méthodes.

## Comment faire:
Voici comment créer un fichier temporaire en PowerShell:

```PowerShell
# Créer un nom de chemin temporaire
$tempPath = [System.IO.Path]::GetTempFileName()

# Afficher le chemin temporaire
Write-Output $tempPath
```

La sortie ressemblera à ceci:

```PowerShell
C:\Users\USER\AppData\Local\Temp\tmp3A1F.tmp
```

Pour écrire dans ce fichier, utilisez le code suivant:

```PowerShell
# Ecrire dans le fichier temporaire
"Test Data" | Out-File -FilePath $tempPath
```

## Plongeons Plus Profondément
Historiquement, la création de fichiers temporaires est une pratique courante en programmation depuis l'époque de la programmation shell sous Unix. En PowerShell, l'objet System.IO.Path offre des méthodes pour travailler facilement avec des chemins de fichiers, y compris la génération de noms de fichiers temporaires.

Il existe des alternatives à l’utilisation de `GetTempFileName()`, comme l’utilisation de `New-TemporaryFile` qui simplifie le processus:

```PowerShell
# Créer un fichier temporaire
$tempFile = New-TemporaryFile
```

Toutefois, `GetTempFileName()` vous donne plus de contrôle sur le processus de création, y compris la possibilité de spécifier un chemin différent pour le fichier si désiré.

## Voir Aussi
Pour de plus amples informations, consultez les sources suivantes:

1. [System.IO.Path GetTempFileName Method:](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
2. [New-TemporaryFile:](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
3. [Out-File:](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)