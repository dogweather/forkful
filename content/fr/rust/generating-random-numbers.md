---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La génération de nombres aléatoires est le processus de création de nombres qui ne peuvent pas être prévus mieux qu'en les choisissant au hasard. Les programmeurs le font pour différents besoins tels que la simulation de données réelles, la cryptographie, les jeux, l'art et de nombreux autres domaines.

## Comment faire:

Créons un nombre aléatoire en Rust. D'abord, ajoutez ceci à votre `Cargo.toml` :
```Rust
[dependencies]
rand = "0.8.3"
```
Puis, dans votre fichier `.rs` :
```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let number: u32 = rng.gen_range(0..10);
    println!("Votre nombre aléatoire entre 0 et 10 est : {}", number);
}
```

Quand vous exécutez ce programme, vous pouvez obtenir une sortie comme celle-ci :
```
Votre nombre aléatoire entre 0 et 10 est : 7
```

Notez que `gen_range` est utilisé pour générer un nombre entre deux bornes.

## Plongée en profondeur

Historiquement, générer des nombres aléatoires en programmation n'a pas toujours été une tâche simple. Les anciens systèmes utilisaient des techniques comme le bruit de fond de l'électronique ou le temps système. Rust utilise une bibliothèque appelée `rand`, qui utilise une combinaison de techniques modernes pour générer des nombres aléatoires.

Une alternative à la bibliothèque `rand` serait d'appeler directement les API de votre système d'exploitation, mais cela peut être plus complexe et moins portable.

Une chose importante à noter est que la plupart des générateurs de nombres aléatoires en informatique sont en réalité pseudo-aléatoires, ce qui signifie qu'ils utilisent un certain déterminisme mais sont suffisamment imprévisibles pour la plupart des usages.

## Voir aussi

- Documentation Rust `rand`: https://docs.rs/rand/
- Wikipedia sur les nombres aléatoires : https://fr.wikipedia.org/wiki/Nombre_al%C3%A9atoire
- Pour comprendre la pseudorandomité : https://fr.wikipedia.org/wiki/G%C3%A9n%C3%A9rateur_de_nombres_al%C3%A9atoires