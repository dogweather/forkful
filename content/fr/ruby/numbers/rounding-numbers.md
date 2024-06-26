---
date: 2024-01-26 03:47:34.893404-07:00
description: "Comment faire : Arrondir les nombres n'est pas nouveau \u2014 les humains\
  \ le font depuis des si\xE8cles pour faciliter les calculs ou pour travailler dans\
  \ les\u2026"
lastmod: '2024-04-05T22:51:12.271295-06:00'
model: gpt-4-0125-preview
summary: "Arrondir les nombres n'est pas nouveau \u2014 les humains le font depuis\
  \ des si\xE8cles pour faciliter les calculs ou pour travailler dans les limites\
  \ de leurs outils."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
```Ruby
# Arrondi basique
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Spécifier la précision
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Arrondir vers le bas
puts 2.9.floor          # => 2

# Arrondir vers le haut
puts 2.1.ceil           # => 3

# Arrondir vers zéro
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Exemple de sortie :
```
3
3
3.14
2.68
2
3
-3
-2
```

## Plongée en profondeur
Arrondir les nombres n'est pas nouveau — les humains le font depuis des siècles pour faciliter les calculs ou pour travailler dans les limites de leurs outils. Dans Ruby, la méthode `round` est polyvalente, avec la capacité d'arrondir au nombre entier le plus proche par défaut ou à un endroit décimal spécifié.

Une alternative à `round` est `floor` pour toujours arrondir vers le bas, et `ceil` pour toujours arrondir vers le haut, indépendamment de la valeur du nombre. Pour simplement couper les décimales, vous avez `truncate`.

Historiquement, quand il s'agit d'ordinateurs, l'arrondi devient crucial dans le traitement de l'arithmétique à virgule flottante en raison de son imprécision inhérente. Ruby, comme la plupart des langages, suit la norme IEEE 754 pour les nombres à virgule flottante, ce qui signifie qu'il gère l'arrondi de manière que la plupart des programmeurs devraient pouvoir prédire et sur laquelle ils peuvent compter.

Cependant, il y a plus à savoir — des choses comme l'arrondi du banquier (également connu sous le nom d'arrondi à la demi pair) sont des concepts que les développeurs Ruby peuvent avoir besoin d'implémenter manuellement, puisque la méthode `round` ne l'offre pas directement.

## Voir Aussi
- La [Documentation Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) pour la méthode `round` des Floats.
- [Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Comprendre la précision à virgule flottante](https://floating-point-gui.de/), pour une compréhension plus approfondie de la manière dont les ordinateurs gèrent les nombres décimaux.
