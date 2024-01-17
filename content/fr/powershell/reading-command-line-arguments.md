---
title:                "Lecture des arguments de ligne de commande."
html_title:           "PowerShell: Lecture des arguments de ligne de commande."
simple_title:         "Lecture des arguments de ligne de commande."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Lecture des arguments de ligne de commande est un outil essentiel pour les programmeurs PowerShell. Cela permet aux utilisateurs d'entrer des paramètres et des données en utilisant une ligne de commande plutôt que d'interagir avec une interface graphique. Cela rend le processus plus rapide et plus facile pour les utilisateurs avancés.

## Comment faire:
Voici deux exemples de code PowerShell qui illustrent comment lire des arguments de ligne de commande.

```PowerShell
# Exemple 1:
Param(
    [string] $name
)
Write-Host "Bonjour $name!"

# Output:
PS C:\> .\bonjour.ps1 -name "Paul"
Bonjour Paul!
```

```PowerShell
# Exemple 2:
Param(
    [switch] $verbose
)
if ($verbose) {
    Write-Host "Mode verbeux activé!"
}
else {
    Write-Host "Mode verbeux désactivé!"
}

# Output:
PS C:\> .\modeverbeux.ps1 -verbose
Mode verbeux activé!
PS C:\> .\modeverbeux.ps1
Mode verbeux désactivé!
```

## Plongeons plus profondément:
La lecture des arguments de ligne de commande est une pratique courante dans la programmation, datant de l'époque des interfaces CLI (Command Line Interface). Les programmeurs utilisent souvent des alternatives telles que les formulaires interactifs ou les GUI, mais la commande line reste une option préférée pour de nombreux utilisateurs avancés.

## Voir aussi:
Pour en savoir plus sur la lecture des arguments de ligne de commande en PowerShell, vous pouvez consulter les ressources suivantes:
- [Documentation officielle de Microsoft sur les arguments de ligne de commande en PowerShell](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7)
- [Tutoriel sur les arguments de ligne de commande en PowerShell sur le blog des développeurs Microsoft](https://devblogs.microsoft.com/scripting/learn-how-to-use-the-powershell-automatic-variable-commandline/)
- [Guide des bonnes pratiques pour la lecture des arguments de ligne de commande en PowerShell](https://powershell.org/2012/12/08/powershell-best-practices-argument-leveraging/)