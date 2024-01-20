---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La génération de nombres aléatoires est une fonctionnalité couramment utilisée en programmation. Les programmeurs l'utilisent pour créer des données d'essai, ajouter de l'aléatoire dans les jeux ou générer une valeur secrète, comme un token de sécurité.

## Comment faire:

Pour générer un nombre aléatoire en PowerShell, on utilise souvent le Get-Random. Voici un exemple rapide:

```PowerShell
# Génération d'un nombre aléatoire entre 1 et 100
$nombreRandom = Get-Random -Minimum 1 -Maximum 100
Write-Output $nombreRandom
```

Quand vous exécutez ce script, vous verrez un nombre aléatoire entre 1 et 100. Par exemple, lors d'une exécution vous pouvez avoir l'affichage suivant:

```PowerShell
56
```

## Plongée en profondeur:

Historiquement, les programmeurs utilisaient des méthodes comme le lancer de dés ou les mouvements de souris pour générer de l'aléatoire. Maintenant, la plupart des langages de programmation, dont PowerShell, disposent de fonctions intégrées pour générer des nombres aléatoires. 

Une alternative à Get-Random est l'utilisation d'une fonction basée sur le .NET comme System.Random. C'est plus complexe mais cela peut être nécessaire si vous recherchez une méthode plus flexible ou spécifique.

Au niveau de l'implémentation, Get-Random utilise un générateur de nombres aléatoires pseudo-aléatoire. Ce n'est pas parfaitement aléatoire, mais suffisamment proche pour la plupart des utilisations.

## Voir aussi:

Pour plus d'informations sur la génération de nombres aléatoires en PowerShell :

- Documentation officielle de `Get-Random` sur le site de Microsoft : [LIEN](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/get-random)