---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "PowerShell: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Vérifier si un répertoire existe fait référence à la vérification de l'existence d'un dossier ou d'un chemin sur votre ordinateur. Les programmeurs le font souvent pour s'assurer que le fichier qu'ils cherchent à utiliser existe avant de continuer avec leur code. Cela évite les erreurs et les bugs potentiels.

## Comment faire:

```PowerShell  
# Vérifier si un dossier existe:
Test-Path C:\Users\Example\Desktop\TestFolder

# Sortie attendue:
True
```

```PowerShell
# Vérifier si un chemin existe:
Test-Path "C:\Users\Example\Desktop\TestFolder\testfile.txt"

# Sortie attendue:
True
```

## Plongée Profonde:

Il est important pour les programmeurs de vérifier l'existence de dossiers et de chemins car cela permet d'éviter les erreurs et les bogues inattendus. Avant PowerShell, il fallait utiliser des commandes spécifiques pour vérifier l'existence d'un dossier ou d'un chemin. Maintenant, la cmdlet Test-Path permet de le faire facilement et rapidement.

Il existe d'autres méthodes pour vérifier l'existence d'un dossier ou d'un chemin, comme l'utilisation de la commande IF, mais Test-Path est la méthode la plus simple et la plus efficace.

## Voir aussi:

- [Comprendre les bases de PowerShell](https://docs.microsoft.com/fr-fr/powershell/scripting/overview?view=powershell-7)
- [Documentation officielle de la cmdlet Test-Path](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.management/test-path?view=powershell-7)