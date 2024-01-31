---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests, c'est créer des mini-programmes pour vérifier que chaque partie de ton code fonctionne comme prévu. Les programmeurs en écrivent pour s'assurer que leur code est fiable et éviter les bugs.

## Comment faire :
Voici un script de test simple avec `Pester`, un framework de test en PowerShell. Installe Pester si ce n'est pas déjà fait :

```PowerShell
Install-Module -Name Pester -Force -SkipPublisherCheck
```

Ensuite, crée un fichier de test `MonScript.Tests.ps1` avec ce contenu :

```PowerShell
Describe "Test de ma fonction" {
    It "additionne deux nombres correctement" {
        # Supposons que tu as une fonction appelée Ajouter
        Ajouter 2 3 | Should -Be 5
    }
}
```

Exécute le test :

```PowerShell
Invoke-Pester .\MonScript.Tests.ps1
```

Tu verras un résultat comme cela :

```
Starting discovery in 1 files.
Discovery finished in xxx seconds.
Running tests from 'MonScript.Tests.ps1'
Describing Test de ma fonction
  [+] additionne deux nombres correctement xxx ms (xxx ms|xxx ms)
Tests completed in xxx seconds
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Exploration approfondie
Pester existe depuis PowerShell v3 et s'est imposé comme le standard pour tests. Il propose une syntaxe claire et des commandes comme `Describe`, `Context`, `It`, facilitant l'organisation des tests. Alternativement, tu peux utiliser PSUnit ou d'autres, mais Pester reste le plus populaire. L'implémentation repose sur les assertions pour évaluer le comportement du code. Pense à 'Should' pour vérifier le résultat attendu.

## Voir aussi
Pour approfondir, visite [la documentation officielle de Pester](https://pester.dev/docs/quick-start) et pour des concepts plus généraux sur les tests, [Tester est doux (eh oui, c’est vrai)](https://testing.googleblog.com/2012/01/testing-on-toilet-testing-is-doc.html).
