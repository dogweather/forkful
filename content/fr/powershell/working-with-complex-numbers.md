---
title:                "Manipulation des nombres complexes"
aliases:
- fr/powershell/working-with-complex-numbers.md
date:                  2024-01-26T04:44:02.681944-07:00
model:                 gpt-4-0125-preview
simple_title:         "Manipulation des nombres complexes"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Les nombres complexes, ceux ayant une partie réelle et une partie imaginaire (comme 3 + 4i), sont essentiels dans des domaines tels que l'ingénierie, la physique et la science des données. Les programmeurs les utilisent pour des simulations, le traitement de signal et la résolution de types spécifiques de problèmes mathématiques.

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
