---
date: 2024-01-26 04:44:02.681944-07:00
description: "Comment faire : PowerShell n'a pas de support int\xE9gr\xE9 pour les\
  \ nombres complexes, donc vous devez soit d\xE9velopper votre propre solution, soit\
  \ utiliser\u2026"
lastmod: '2024-03-13T22:44:58.037373-06:00'
model: gpt-4-0125-preview
summary: "PowerShell n'a pas de support int\xE9gr\xE9 pour les nombres complexes,\
  \ donc vous devez soit d\xE9velopper votre propre solution, soit utiliser `System.Numerics.Complex`\
  \ de .NET."
title: Manipulation des nombres complexes
weight: 14
---

## Comment faire :
PowerShell n'a pas de support intégré pour les nombres complexes, donc vous devez soit développer votre propre solution, soit utiliser `System.Numerics.Complex` de .NET.

```PowerShell
# Créons des nombres complexes en utilisant .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Créer des nombres complexes
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Additionner deux nombres complexes
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiplier deux nombres complexes
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Afficher les résultats
"Somme : $sum"
"Produit : $product"
```
Sortie :
```
Somme : (4, 6)
Produit : (-5, 10)
```

## Exploration Approfondie
Les nombres complexes ont été développés au 16e siècle pour résoudre des équations qui n'avaient pas de solutions dans le domaine des nombres réels. Ils constituent maintenant une pierre angulaire des mathématiques modernes.

La dépendance de PowerShell à .NET pour le support des nombres complexes signifie que la performance est solide. Les alternatives incluent des bibliothèques tierces ou d'autres langages de programmation comme Python, où les nombres complexes sont un type de données natif.

## Voir Aussi
- [Structure System.Numerics.Complex](https://docs.microsoft.com/fr-fr/dotnet/api/system.numerics.complex)
- [Arithmétique des nombres complexes en Python](https://docs.python.org/3/library/cmath.html)
