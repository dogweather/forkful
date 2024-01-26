---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:57.233702-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La génération de nombres aléatoires sert à créer des données imprévisibles. Les développeurs l'utilisent pour les jeux, les simulations, et là où l'onécessite de l'entropie.

## Comment faire :
```gleam
import gleam/erlang
import gleam/int

pub fn main() {
  // Générer un nombre aléatoire entre 0 et 99
  let rand_num = erlang.rand_uniform(100)
  int.to_string(rand_num)
}
```
Sortie d’exemple : `"42"`

## Exploration en profondeur :
La génération de nombres aléatoires n'est pas vraiment "aléatoire" en informatique ; on parle de pseudo-aléatoire parce qu'elle suit un algorithme. Historiquement, les algorithmes comme le générateur congruentiel linéaire (LCG) étaient populaires, mais avaient des faiblesses. Aujourd'hui, on préfère des méthodes plus robustes comme Mersenne Twister, bien que Erlang utilise son propre système. En Gleam, qui est construit sur la VM d'Erlang, on accède à la fonctionnalité aléatoire à travers le module `erlang`. Attention, ceci n'est pas destiné à des usages cryptographiques, pour lesquels il faudrait des générateurs aléatoires cryptographiquement sécurisés.

## Voir également :
- Pour une sécurité accrue, RNGs cryptographiques : [https://en.wikipedia.org/wiki/Cryptographically-secure_pseudorandom_number_generator](https://en.wikipedia.org/wiki/Cryptographically-secure_pseudorandom_number_generator)
- Un aperçu des générateurs de nombres pseudo-aléatoires : [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
