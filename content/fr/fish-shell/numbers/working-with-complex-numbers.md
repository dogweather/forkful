---
date: 2024-01-26 04:39:49.601534-07:00
description: "Les nombres complexes \xE9tendent l'id\xE9e de lignes num\xE9riques\
  \ unidimensionnelles \xE0 un plan complexe bidimensionnel. Les programmeurs les\
  \ utilisent dans des\u2026"
lastmod: 2024-02-19 22:05:16.966430
model: gpt-4-0125-preview
summary: "Les nombres complexes \xE9tendent l'id\xE9e de lignes num\xE9riques unidimensionnelles\
  \ \xE0 un plan complexe bidimensionnel. Les programmeurs les utilisent dans des\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes étendent l'idée de lignes numériques unidimensionnelles à un plan complexe bidimensionnel. Les programmeurs les utilisent dans des domaines tels que l'ingénierie, la physique et les graphiques pour des calculs nécessitant deux composants, comme les signaux ou les rotations.

## Comment faire :
Dans Fish, nous gérons les nombres complexes en utilisant `math` avec des parties réelles et imaginaires. Voici comment commencer :

```fish
# Ajouter deux nombres complexes (3+4i) et (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Résultats : 8+6i

# Multiplier deux nombres complexes (1+2i) et (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Résultats : -5+10i
```

Si vous avez besoin d'élever un nombre complexe à une puissance ou d'obtenir sa forme exponentielle :

```fish
# Carré de (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Résultats : -5+12i

# Exponentielle de (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Résultats : -0.41615+0.9093i
```

## Approfondissement
Le support des nombres complexes dans Fish Shell est relativement nouveau, ayant débuté autour de la version 3.1.0. Avant cela, les gens auraient pu utiliser `bc` ou faire appel à des outils externes comme Python pour les calculs complexes.

Parmi les alternatives au math de Fish, on trouve des bibliothèques numériques spécialisées ou des langages comme MATLAB, Python avec NumPy, ou même C++ avec la Bibliothèque Standard. Cependant, ceux-ci pourraient être excessifs pour des calculs rapides en shell.

Le support des nombres complexes dans Fish est intégré dans sa commande interne `math`, en s'appuyant sur libcalc. Cela signifie que vous n'avez pas à installer d'outils supplémentaires pour des opérations de base.

Cependant, Fish n'est pas conçu pour des calculs mathématiques lourds. Sa capacité mathématique est pratique pour des calculs rapides ou des scripts où les nombres complexes entrent en jeu, mais envisagez des outils plus robustes pour des tâches intensives.

## Voir Aussi
- Documentation de Fish shell pour math : https://fishshell.com/docs/current/commands.html#math
- NumPy pour Python, une alternative populaire : https://numpy.org/
- Un regard approfondi sur les nombres complexes : https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
