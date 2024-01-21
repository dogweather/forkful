---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:44.725275-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi?)
Générer des nombres aléatoires, c'est comme lancer un dé virtuel. Dans la programmation, ça sert à tester des trucs, simuler des données ou ajouter du hasard pour que les choses soient moins prévisibles.

## How to: (Comment faire:)
```PowerShell
# Pour un nombre aléatoire simple entre 0 et 100
$randomNumber = Get-Random -Minimum 0 -Maximum 101
Write-Host "Nombre aléatoire entre 0 et 100 : $randomNumber"

# Pour une séquence de nombres aléatoires
$randomSequence = 1..10 | ForEach-Object { Get-Random -Minimum 0 -Maximum 101 }
Write-Host "Séquence de nombres aléatoires : $randomSequence"

# Utiliser System.Random pour plus de contrôle
$randomGenerator = New-Object System.Random
$randomNumberFromGenerator = $randomGenerator.Next(0, 101)
Write-Host "Nombre aléatoire avec System.Random : $randomNumberFromGenerator"
```
Sample Output:
```
Nombre aléatoire entre 0 et 100 : 37
Séquence de nombres aléatoires : 45 23 56 78 81 13 67 89 26 10
Nombre aléatoire avec System.Random : 52
```

## Deep Dive (Plongée en Profondeur)
Les nombres aléatoires en informatique ne sont pas vraiment "aléatoires". Ils sont plutôt "pseudo-aléatoires", car ils dépendent d'un algorithme. Historiquement, différents algorithmes ont été utilisés pour améliorer la qualité de la randomisation.

PowerShell utilise la classe `System.Random` de .NET pour générer ces nombres. `Get-Random` est plus simple pour un usage courant, tandis que `System.Random` offre plus d'options pour les besoins complexes.

Notez que pour de la cryptographie, on utilise `System.Security.Cryptography.RandomNumberGenerator`, car il est plus sécurisé et imprévisible.

## See Also (Voir Aussi)
- [Get-Random documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.2)
- [About Random Numbers](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- [RandomNumberGenerator Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.randomnumbergenerator?view=net-6.0)