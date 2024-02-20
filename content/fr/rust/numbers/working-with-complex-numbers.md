---
date: 2024-01-26 04:45:14.502187-07:00
description: "Les nombres complexes comportent une partie r\xE9elle et une partie\
  \ imaginaire et sont cruciaux dans divers domaines tels que l'ing\xE9nierie, la\
  \ physique et\u2026"
lastmod: 2024-02-19 22:05:16.304001
model: gpt-4-0125-preview
summary: "Les nombres complexes comportent une partie r\xE9elle et une partie imaginaire\
  \ et sont cruciaux dans divers domaines tels que l'ing\xE9nierie, la physique et\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les nombres complexes comportent une partie réelle et une partie imaginaire et sont cruciaux dans divers domaines tels que l'ingénierie, la physique et les graphiques informatiques. Les programmeurs les utilisent pour résoudre des équations que les nombres réels ordinaires ne peuvent pas gérer.

## Comment faire :
Rust n'a pas de support intégré pour les nombres complexes, mais des crates comme `num-complex` vous couvrent. Voici comment l'utiliser :

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let somme = a + b;
    let produit = a * b;

    println!("Somme : {}", somme); // Somme : 3 - 1i
    println!("Produit : {}", produit); // Produit : 14 - 5i
}
```
Vous devrez ajouter `num_complex` à votre `Cargo.toml` pour que cette magie opère.

## Plongée profonde
Les nombres complexes ont été conçus au 16ème siècle mais ont vraiment décollé au 18ème siècle lorsque des mathématiciens comme Euler ont commencé à jouer avec eux.

Sans opérations natives sur les nombres complexes, des langages comme Rust s'appuient sur des bibliothèques tierces. `num-complex` est l'une de ces crates et fait partie de la collection de crates `num` qui vise à fournir des types numériques et des traits pour Rust.

Il convient de mentionner que certains langages (comme Python) ont un support intégré pour les nombres complexes, tandis que d'autres (comme C++, avec l'en-tête `<complex>`) les fournissent comme partie de la bibliothèque standard. Dans Rust, la décision de garder la bibliothèque standard petite signifie que vous vous tournerez souvent vers des crates créées par la communauté pour des fonctionnalités supplémentaires.

## Voir également
- [Livre Rust](https://doc.rust-lang.org/book/) : Pour en savoir plus sur Rust et comment travailler avec des crates externes.
- [Nombre complexe Wikipedia](https://fr.wikipedia.org/wiki/Nombre_complexe) : Pour une compréhension plus approfondie des nombres complexes.
