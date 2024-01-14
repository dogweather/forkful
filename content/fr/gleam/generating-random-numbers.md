---
title:    "Gleam: La génération de nombres aléatoires"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une fonctionnalité utile dans de nombreux cas, que ce soit pour des jeux, des simulations ou même des tests. Cela permet d'introduire une certaine imprévisibilité dans nos programmes et de les rendre plus dynamiques.

## Comment Faire

Gleam, un langage de programmation fonctionnel à typage statique, offre une façon simple et efficace de générer des nombres aléatoires. Voici un exemple de code qui génère dix nombres entiers aléatoires entre 1 et 100 :

```
Gleam
import gleam/random
fn main() {
  let rng = random.seed(42)
  let numbers = List.range(1, 10) |> List.map(_ => random.int(1, 100, rng))
  io.format("Nombres aléatoires : {:?}\n", [numbers])
}
```

Voici un exemple de sortie possible :

```
Nombres aléatoires : [94, 16, 28, 87, 7, 64, 35, 39, 95, 52]
```

## Plongée en Profondeur

Gleam utilise le générateur de nombres aléatoires PCG (Permuted Congruential Generator) qui est considéré comme l'un des meilleurs générateurs actuels. Il garantit une distribution uniforme des nombres générés et une période très longue (plusieurs milliards de milliards de milliards). De plus, Gleam inclut également des fonctions pour générer des nombres aléatoires de différentes distributions telles que les nombres réels, les booléens ou même des chaînes de caractères aléatoires.

## Voir Aussi

- Documentation officielle de Gleam sur la génération de nombres aléatoires : https://gleam.run/book/standard-library.html#random-numbers
- Tutoriel sur la manipulation de nombres aléatoires en Gleam : https://medium.com/@shritesh/taking-a-walk-on-random-numbers-in-gleam-1c1cbcbf8b32
- Exemples de génération de nombres aléatoires dans Gleam sur GitHub : https://github.com/search?q=language%3Agleam+random+numbers&type=Repositories