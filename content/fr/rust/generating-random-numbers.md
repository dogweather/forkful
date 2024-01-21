---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:50:00.056119-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Générer des chiffres aléatoires, c'est comme lancer un dé virtuel. On le fait pour la sécurité (pensez aux clés cryptographiques), les jeux, les simulations, ou l'IA. C'est vital pour beaucoup de programmes.

## Comment faire :
```Rust
use rand::{Rng, thread_rng};

fn main() {
    // Créer un générateur de nombres aléatoires
    let mut rng = thread_rng();
    
    // Générer un entier aléatoire
    let random_number: i32 = rng.gen();
    println!("Nombre aléatoire: {}", random_number);
    
    // Générer un flottant aléatoire entre 0 et 1
    let random_float: f64 = rng.gen();
    println!("Flottant aléatoire: {}", random_float);
    
    // Générer un booléen aléatoire
    let random_bool: bool = rng.gen();
    println!("Booléen aléatoire: {}", random_bool);
}
```
Sortie possible:
```
Nombre aléatoire: 183956729
Flottant aléatoire: 0.445634127
Booléen aléatoire: true
```
Notez: Ajoutez `rand` comme dépendance dans `Cargo.toml`.

## Exploration plus profonde :
À l'origine, les ordinateurs étaient mauvais pour l'aléatoire, car ils sont déterministes. Mais on a conçu des méthodes, comme les générateurs de nombres pseudo-aléatoires (PRNGs). En Rust, la bibliothèque `rand` est le standard pour ces tâches. Elle utilise différentes méthodes, comme le chaînage linéaire ou les algorithmes Mersenne Twister, pour simuler l'aléatoire. Alors que `rand` couvre la plupart des besoins, on pourrait explorer `ring` ou `sodiumoxide` pour un usage cryptographique, parce qu'ils fournissent de l'aléatoire cryptographiquement sécurisé.

## Voir aussi :
- La documentation de `rand`: https://docs.rs/rand
- Un guide sur l'aléatoire en Rust : https://rust-lang-nursery.github.io/rust-cookbook/algorithms/randomness.html
- Pour en savoir plus sur PRNGs : https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- `ring`, une bibliothèque de sécurité : https://crates.io/crates/ring
- `sodiumoxide`, des liaisons Rust pour libsodium : https://crates.io/crates/sodiumoxide