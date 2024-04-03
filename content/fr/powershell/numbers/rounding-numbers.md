---
date: 2024-01-26 03:46:19.473332-07:00
description: "Comment faire : Vous disposez de quelques cmdlets et m\xE9thodes pratiques\
  \ dans PowerShell pour l'arrondi: - m\xE9thode `Round()` de la classe Math."
lastmod: '2024-03-13T22:44:58.038545-06:00'
model: gpt-4-0125-preview
summary: "Vous disposez de quelques cmdlets et m\xE9thodes pratiques dans PowerShell\
  \ pour l'arrondi."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
Vous disposez de quelques cmdlets et méthodes pratiques dans PowerShell pour l'arrondi:

- méthode `Round()` de la classe Math
```PowerShell
[Math]::Round(15.68) # Arrondit à 16
```
- Spécifier les décimales :
```PowerShell
[Math]::Round(15.684, 2) # Arrondit à 15.68
```
- `Ceiling()` et `Floor()`, pour arrondir toujours à l'entier supérieur ou inférieur :
```PowerShell
[Math]::Ceiling(15.2) # Arrondit à 16
[Math]::Floor(15.9) # Arrondit à 15
```

## Plongée Profonde
L'arrondi des nombres n'est pas une nouveauté ; il existe depuis l'antiquité, utile pour le commerce, la science et la mesure du temps. Parlant de PowerShell, `[Math]::Round()` suit par défaut l'"Arrondi Bancaire", où les 0.5 vont à l'entier pair le plus proche, réduisant le biais dans les opérations statistiques.

Vous n'êtes pas seulement limité aux méthodes `[Math]`. Vous voulez plus de contrôle ? Consultez `[System.Math]::Round(Number, Digits, MidpointRounding)` où vous pouvez définir comment les points médians sont traités : à l'écart de zéro ou vers l'entier pair (alias Arrondi Bancaire).

Un autre angle : l'objet `System.Globalization.CultureInfo`. Il aide à la mise en forme spécifique à la locale et aux préférences d'arrondi lors de la manipulation de nombres internationaux.

## Voir Aussi
- Docs officiels de Microsoft sur les méthodes Math : [Lien](https://learn.microsoft.com/fr-fr/dotnet/api/system.math?view=net-7.0)
- Spécificités de l'arrondi décimal dans .NET : [Lien](https://learn.microsoft.com/fr-fr/dotnet/api/system.midpointrounding?view=net-7.0)
- Discussions sur l'arrondi dans StackOverflow : [Lien](https://stackoverflow.com/questions/tagged/rounding+powershell)
