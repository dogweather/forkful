---
title:                "Rust: La génération de nombres aléatoires"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires en Rust ?

Vous vous demandez peut-être pourquoi quelqu'un voudrait générer des nombres aléatoires en Rust. Eh bien, mis à part le fait que c'est amusant, cela peut également être très utile pour des tâches telles que la génération de données aléatoires pour les tests, la simulation de situations aléatoires dans un programme ou même la création de jeux.

## Comment le faire en Rust

Générer des nombres aléatoires en Rust est assez simple grâce à la bibliothèque standard. Tout d'abord, vous devrez importer la bibliothèque en ajoutant `extern crate rand;` en haut de votre fichier. Ensuite, vous pouvez utiliser la fonction `random()` pour générer un nombre aléatoire entre 0 et un nombre maximum spécifié. Par exemple :

```Rust
use rand::Rng; // importer la bibliothèque

let number = rand::thread_rng().gen_range(1..101); // générer un nombre aléatoire entre 1 et 100

println!("Mon nombre aléatoire est : {}", number); // afficher le nombre généré
```

En utilisant `thread_rng()`, nous nous assurons que chaque fois que notre programme est exécuté, un nouveau générateur de nombres aléatoires est créé, assurant ainsi des valeurs réellement aléatoires.

## Plongée en profondeur

Maintenant, si vous voulez aller encore plus loin dans la génération de nombres aléatoires en Rust, il existe également une variété d'options pour personnaliser votre générateur. Par exemple, vous pouvez spécifier le type de données que vous souhaitez générer, comme des nombres entiers, des décimaux, des caractères, etc. Vous pouvez également définir une graine pour votre générateur, ce qui permet de reproduire les mêmes valeurs aléatoires à chaque exécution. Enfin, il existe également des méthodes pour générer des séquences aléatoires, des nombres binaires et même des valeurs pseudo-aléatoires basées sur une distribution spécifique.

Pour en savoir plus sur toutes ces options et leurs utilisations en détail, vous pouvez consulter la documentation complète de la bibliothèque `rand`.

## Voir aussi

- [La documentation officielle de la bibliothèque `rand`](https://doc.rust-lang.org/rand/rand/index.html)
- [Un tutoriel complet sur la génération de nombres aléatoires en Rust](https://medium.com/swlh/random-number-generation-in-rust-27a0b0d38711)
- [Un article sur l'utilisation de générateurs de nombres aléatoires dans les jeux en Rust](https://ashleygwilliams.github.io/fun-with-async-std-and-crypto/)

Maintenant que vous avez appris comment générer des nombres aléatoires en Rust, pourquoi ne pas l'essayer dans votre prochain projet ? Amusez-vous bien !