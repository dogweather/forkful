---
date: 2024-01-27 20:34:48.187481-07:00
description: "Comment faire : PowerShell offre une approche simple pour g\xE9n\xE9\
  rer des nombres al\xE9atoires \xE0 l'aide de l'applet de commande `Get-Random`.\
  \ Cette applet de\u2026"
lastmod: '2024-03-13T22:44:58.039659-06:00'
model: gpt-4-0125-preview
summary: "PowerShell offre une approche simple pour g\xE9n\xE9rer des nombres al\xE9\
  atoires \xE0 l'aide de l'applet de commande `Get-Random`."
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
weight: 12
---

## Comment faire :
PowerShell offre une approche simple pour générer des nombres aléatoires à l'aide de l'applet de commande `Get-Random`. Cette applet de commande peut produire des nombres aléatoires dans une plage par défaut ou une plage spécifiée.

```PowerShell
# Générer un nombre aléatoire entre 0 et Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Pour spécifier une plage, utilisez les paramètres `-Minimum` et `-Maximum` :

```PowerShell
# Générer un nombre aléatoire entre 1 et 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Pour plus de contrôle, vous pouvez instancier un objet de la classe `System.Random` :

```PowerShell
# Utiliser System.Random pour une séquence de nombres
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Si vous avez besoin d'une sélection aléatoire à partir d'un tableau ou d'une collection, `Get-Random` peut directement choisir un élément :

```PowerShell
# Sélection aléatoire à partir d'un tableau
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Plongée en profondeur
L'applet de commande `Get-Random` dans PowerShell utilise sous le capot la classe .NET `System.Random` pour générer des nombres pseudoaléatoires. Ce sont des "pseudo" car ils utilisent des algorithmes pour produire des séquences de nombres qui semblent seulement aléatoires. Pour la plupart des applications, ce niveau d'aléatoire est suffisant. Cependant, pour les cas d'utilisation nécessitant une sécurité cryptographique, `System.Random` n'est pas adapté en raison de sa nature prévisible.

PowerShell et .NET offrent `System.Security.Cryptography.RNGCryptoServiceProvider` pour l'aléatoire cryptographique, qui est plus approprié pour générer des clés de chiffrement ou d'autres opérations sensibles à la sécurité :

```PowerShell
# Nombres aléatoires cryptographiquement sécurisés
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Bien que `Get-Random` et `System.Random` répondent à un large éventail de besoins en matière d'aléatoire dans les scripts et la logique des applications, il est essentiel de choisir l'outil adéquat pour le travail, en particulier dans les applications centrées sur la sécurité où la prévisibilité peut représenter une vulnérabilité.
