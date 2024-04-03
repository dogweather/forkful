---
date: 2024-01-26 03:46:39.596407-07:00
description: "Arrondir des nombres signifie les ajuster au nombre entier le plus proche\
  \ ou \xE0 une fraction avec une certaine pr\xE9cision. Les programmeurs arrondissent\
  \ les\u2026"
lastmod: '2024-03-13T22:44:57.476720-06:00'
model: gpt-4-0125-preview
summary: "Arrondir des nombres signifie les ajuster au nombre entier le plus proche\
  \ ou \xE0 une fraction avec une certaine pr\xE9cision."
title: Arrondir les nombres
weight: 13
---

## Quoi & Pourquoi ?
Arrondir des nombres signifie les ajuster au nombre entier le plus proche ou à une fraction avec une certaine précision. Les programmeurs arrondissent les nombres pour simplifier les valeurs pour la lisibilité humaine, pour répondre aux exigences des spécifications, ou pour réduire la charge de calcul dans les opérations à virgule flottante.

## Comment faire :
Rust rend l'arrondi très simple. Découvrez ces méthodes pour les types `f32` ou `f64` :

```rust
fn main() {
    let num = 2.34567;

    // Arrondir au nombre entier le plus proche
    let round = num.round();
    println!("Arrondi: {}", round); // Arrondi: 2

    // Plancher - le plus grand entier inférieur ou égal au nombre
    let floor = num.floor();
    println!("Plancher: {}", floor); // Plancher: 2

    // Plafond - le plus petit entier supérieur ou égal au nombre
    let ceil = num.ceil();
    println!("Plafond: {}", ceil); // Plafond: 3

    // Tronquer - la partie entière sans les chiffres fractionnaires
    let trunc = num.trunc();
    println!("Tronquer: {}", trunc); // Tronquer: 2

    // Au multiple le plus proche d'une puissance de dix
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Arrondi à 2 décimales: {}", multiple_of_ten); // Arrondi à 2 décimales: 2.35
}
```

## Plongée profonde
Historiquement, l'arrondi a été crucial pour adapter des décimales infinies ou des nombres irrationnels dans des espaces numériques limités - indispensable pour les anciens ordinateurs avec peu de mémoire. Pensez à un abaque mais moins artisanal, plus mathématique.

Les alternatives aux méthodes natives de Rust incluent :
1. La macro `format!` pour la mise en forme des chaînes qui arrondit par défaut.
2. Les crates externes pour des tâches mathématiques spécialisées, comme la crate `round` avec un contrôle plus granulaire.

Sous le capot, les opérations d'arrondi de Rust sont conformes aux normes IEEE - jargon technique pour "cela arrondit comme votre prof de maths le souhaite". De plus, en raison des représentations binaires, certains nombres ne peuvent pas être arrondis de manière traditionnelle, comme 0.1, en raison de leur représentation infinie en binaire.

## Voir aussi
- Documentation Rust sur les méthodes de type primitif : https://doc.rust-lang.org/std/primitive.f64.html
- Norme IEEE pour l'arithmétique à virgule flottante (IEEE 754) : https://ieeexplore.ieee.org/document/4610935
- Crate "round" pour un arrondi plus complexe : https://crates.io/crates/round
