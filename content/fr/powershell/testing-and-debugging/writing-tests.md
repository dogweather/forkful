---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.888030-07:00
description: "Comment faire : PowerShell n'a pas de framework de test int\xE9gr\xE9\
  , mais Pester, un module tiers populaire, est largement utilis\xE9 pour \xE9crire\
  \ et ex\xE9cuter des\u2026"
lastmod: '2024-03-13T22:44:58.052315-06:00'
model: gpt-4-0125-preview
summary: "PowerShell n'a pas de framework de test int\xE9gr\xE9, mais Pester, un module\
  \ tiers populaire, est largement utilis\xE9 pour \xE9crire et ex\xE9cuter des tests."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
PowerShell n'a pas de framework de test intégré, mais Pester, un module tiers populaire, est largement utilisé pour écrire et exécuter des tests. Voici comment commencer avec Pester pour tester vos fonctions PowerShell.

Premièrement, installez Pester si vous ne l'avez pas déjà fait :

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Ensuite, considérez que vous avez une simple fonction PowerShell que vous souhaitez tester, sauvegardée en tant que `MyFunction.ps1` :

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Pour tester cette fonction avec Pester, créez un script de test nommé `MyFunction.Tests.ps1`. Dans ce script, utilisez les blocs `Describe` et `It` de Pester pour définir les cas de test :

```powershell
# Importer la fonction à tester
. .\MyFunction.ps1

Describe "Tests de Get-MultipliedNumber" {
    It "Multiplie le nombre par 2 quand aucun multiplicateur n'est fourni" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multiplie correctement le nombre par le multiplicateur donné" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Pour exécuter les tests, ouvrez PowerShell, naviguez jusqu'au répertoire contenant votre script de test, et utilisez la commande `Invoke-Pester` :

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

Le résultat ressemblera à ceci, indiquant si vos tests ont réussi ou échoué :

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\chemin\vers\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

Ce résultat montre que les deux tests ont réussi, vous donnant confiance dans le fait que votre fonction `Get-MultipliedNumber` se comporte comme attendu dans les scénarios que vous avez testés.
