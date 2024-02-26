---
date: 2024-01-26 04:45:03.225027-07:00
description: "Les nombres complexes, compos\xE9s d'une partie r\xE9elle et d'une partie\
  \ imaginaire (comme 3+4i), sont un pilier dans l'ing\xE9nierie et la physique. Les\u2026"
lastmod: '2024-02-25T18:49:55.034500-07:00'
model: gpt-4-0125-preview
summary: "Les nombres complexes, compos\xE9s d'une partie r\xE9elle et d'une partie\
  \ imaginaire (comme 3+4i), sont un pilier dans l'ing\xE9nierie et la physique. Les\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes, composés d'une partie réelle et d'une partie imaginaire (comme 3+4i), sont un pilier dans l'ingénierie et la physique. Les programmeurs les utilisent dans des simulations, le traitement de signal et la résolution d'équations qui ne fonctionnent pas bien avec seulement des nombres réels.

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
