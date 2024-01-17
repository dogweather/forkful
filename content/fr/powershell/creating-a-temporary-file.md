---
title:                "Créer un fichier temporaire"
html_title:           "PowerShell: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi créer un fichier temporaire?

Créer un fichier temporaire est une pratique courante chez les programmeurs. Il s'agit simplement de créer un fichier qui est supprimé une fois que son utilisation est terminée. Les programmeurs utilisent souvent cette méthode pour stocker temporairement des données ou pour créer des copies de fichiers existants.

## Comment le faire:

Utilisez la commande ```PowerShell New-TemporaryFile``` pour créer un nouveau fichier temporaire. Voici un exemple de code et sa sortie :

```PowerShell
New-TemporaryFile -Name "tempfile.txt" | Format-List -Property FullName, Extension

FullName   : C:\Users\Username\AppData\Local\Temp\tmpAF21.txt
Extension : .txt
```

Vous pouvez également spécifier un emplacement spécifique pour votre fichier temporaire en utilisant le paramètre ```Path```:

```PowerShell
New-TemporaryFile -Name "tempfile.txt" -Path "C:\Users\Username\Desktop" | Format-List -Property FullName, Extension

FullName   : C:\Users\Username\Desktop\tmpAF21.txt
Extension : .txt
```

## Plongée en profondeur :

La pratique de créer des fichiers temporaires remonte aux premiers jours de l'informatique lorsque les programmes étaient exécutés sur des cartes perforées. Aujourd'hui, il existe plusieurs alternatives à la création de fichiers temporaires comme l'utilisation de variables en mémoire, mais créer un fichier temporaire reste une méthode fiable et efficace pour stocker temporairement des données.

L'implémentation de base de la commande ```New-TemporaryFile``` est similaire à l'utilisation de la commande ```New-Item``` pour créer un nouveau fichier. Cependant, elle inclut également la suppression automatique du fichier après son utilisation.

## À voir aussi :

- [Documentation Microsoft sur la commande New-TemporaryFile] (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7)
- [Article sur la création de fichiers temporaires avec PowerShell] (https://www.scriptblocks.org/wpnew/2019/07/creation-de-fichiers-temporaire-avec-powershell/) par ScriptBlocks
- [Forum sur l'utilisation des fichiers temporaires en programmation] (https://stackoverflow.com/questions/1676063/what-is-the-perfect-way-to-create-a-temp-file-in-powershell) sur StackOverflow