---
date: 2024-01-26 03:44:03.195543-07:00
description: 'Comment faire : Dans Fish, l''arrondi des nombres repose sur la commande
  `math`. Utilisez `math -s0` pour arrondir au plus proche entier.'
lastmod: '2024-03-13T22:44:58.316526-06:00'
model: gpt-4-0125-preview
summary: Dans Fish, l'arrondi des nombres repose sur la commande `math`.
title: Arrondir les nombres
weight: 13
---

## Comment faire :
Dans Fish, l'arrondi des nombres repose sur la commande `math`. Utilisez `math -s0` pour arrondir au plus proche entier.

```fish
# Arrondir à l'entier supérieur
echo (math -s0 "4.7")
# Sortie: 5

# Arrondir à l'entier inférieur
echo (math -s0 "4.3")
# Sortie: 4

# Arrondir à deux décimales
echo (math -s2 "4.5678")
# Sortie: 4.57

# Arrondir un nombre négatif
echo (math -s0 "-2.5")
# Sortie: -3
```

## Exploration approfondie
Historiquement, l'arrondi des nombres était effectué de manière plus manuelle ou à l'aide d'outils externes, mais dans les interfaces modernes comme Fish, cela fait partie intégrante des utilitaires intégrés. L'approche de Fish utilisant la commande `math` simplifie les choses par rapport aux anciennes interfaces. Les alternatives dans d'autres environnements de programmation varient ; des langages comme Python utilisent des fonctions telles que `round()`, tandis que Bash pourrait nécessiter des expressions plus complexes ou l'utilitaire `bc`. L'implémentation de l'arrondi dans Fish simplifie le script en gardant les calculs mathématiques à l'intérieur de l'environnement de l'interface au lieu de faire appel à d'autres outils ou langages.

## Voir aussi
- Documentation Fish pour la commande `math` : https://fishshell.com/docs/current/cmds/math.html
- Standard IEEE pour l'arithmétique à virgule flottante (IEEE 754) : https://ieeexplore.ieee.org/document/4610935
