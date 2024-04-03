---
date: 2024-01-26 04:45:03.225027-07:00
description: "Comment faire : Ruby facilite la manipulation des nombres complexes.\
  \ Vous pouvez les cr\xE9er et les manipuler en utilisant la classe Complex ."
lastmod: '2024-03-13T22:44:58.411431-06:00'
model: gpt-4-0125-preview
summary: Ruby facilite la manipulation des nombres complexes.
title: Manipulation des nombres complexes
weight: 14
---

## Comment faire :
Ruby facilite la manipulation des nombres complexes. Vous pouvez les créer et les manipuler en utilisant la classe Complex :

```ruby
require 'complex'

# Créer des nombres complexes
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Opérations de base
somme = c1 + c2               # => (5.0+9.0i)
différence = c1 - c2        # => (1.0-1.0i)
produit = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Conjugué, magnitude et phase
conjugué = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 radians

# Méthodes spécifiques aux complexes
polaire = c1.polar            # => [5.0, 0.9272952180016122]
rectangulaire = c1.rect       # => [3.0, 4.0]
```

## Approfondissement
Les nombres complexes ne sont pas nouveaux - ils existent depuis le 16e siècle, résolvant des équations sans solutions réelles. En dehors des mathématiques, la classe Complex de Ruby réalise le gros du travail computationnel, appuyée par le module Math pour les fonctions trigonométriques et transcendantales.

Les langages de programmation antérieurs nécessitaient une gestion manuelle des parties réelle et imaginaire. Certains, comme Fortran et C++, dédient des bibliothèques spéciales à l'arithmétique complexe.

L'approche de Ruby intègre le support des nombres complexes dans sa syntaxe, vous évitant de réinventer la roue. Derrière le rideau, la classe Complex gère les mathématiques, tandis que Ruby s'occupe des interactions entre objets.

## Voir également
- Docs Ruby sur Complex : [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorld sur les nombres complexes : [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Une introduction visuelle aux nombres complexes et leur utilité : [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
