---
title:                "Rust: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi Générer des Nombres Aléatoires en Rust ?

Si vous êtes programmeur en herbe, vous avez peut-être entendu parler du langage de programmation Rust et de ses performances élevées. Mais pourquoi générer des nombres aléatoires en utilisant ce langage ? Eh bien, simplement parce que Rust offre une puissante bibliothèque de génération de nombres aléatoires incorporée dans sa bibliothèque standard, appelée "rand". Cela signifie que vous n'avez pas besoin d'utiliser des bibliothèques tierces ou de créer votre propre algorithme pour générer des nombres aléatoires, tout est déjà intégré dans Rust.

## Comment Générer des Nombres Aléatoires en Rust ?

La génération de nombres aléatoires en utilisant Rust est assez simple et directe. Tout d'abord, vous devez importer la bibliothèque "rand" en utilisant la déclaration `use rand::prelude::*;`. Ensuite, vous pouvez utiliser l'une des structures de données de la bibliothèque pour générer des nombres aléatoires selon vos besoins. Par exemple, pour générer un nombre entier aléatoire entre 0 et 100 inclus, vous pouvez utiliser la méthode `gen_range` de la structure `ThreadRng` comme ceci :

```Rust
let number = thread_rng().gen_range(0, 101);
println!("Nombre aléatoire : {}", number);
```

Cette méthode prend deux paramètres, la valeur minimale et la valeur maximale entre lesquelles vous voulez générer le nombre aléatoire. Il existe également d'autres méthodes disponibles pour générer des nombres aléatoires, comme `gen`, `gen_bool` et `gen_ascii_chars`. Pour en savoir plus sur ces différentes méthodes, vous pouvez vous référer à la [documentation officielle de la bibliothèque rand](https://docs.rs/rand/0.8.4/rand/).

## Plongée Profonde : Génération de Nombres Aléatoires en Rust

Si vous voulez en savoir plus sur la génération de nombres aléatoires en utilisant Rust, il est important de comprendre comment les nombres aléatoires sont générés réellement. En fait, la bibliothèque "rand" utilise un générateur de nombres pseudo-aléatoires basé sur l'algorithme Mersenne Twister (MT). C'est l'un des générateurs de nombres aléatoires les plus couramment utilisés et il a une période de 2^19937 - 1, ce qui signifie que les nombres générés peuvent être uniques pour une très longue période. Si vous souhaitez en savoir plus sur cet algorithme, vous pouvez consulter [cet article](https://medium.com/swlh/how-are-random-numbers-generated-in-programming-48f6e53e5d6e) qui explique en détail comment les nombres pseudo-aléatoires sont générés.

# Voir Aussi

Maintenant que vous en savez plus sur la génération de nombres aléatoires en Rust, vous pouvez explorer d'autres fonctionnalités et bibliothèques offertes par ce langage de programmation puissant. Voici quelques liens utiles pour vous aider à démarrer :

- [Le site officiel de Rust](https://www.rust-lang.org/fr)
- [La documentation officielle de Rust](https://doc.rust-lang.org/)
- [La documentation officielle de la bibliothèque rand](https://docs.rs/rand/0.8.4/rand/)
- [Une introduction à Rust pour les programmeurs Python](https://blog.rust-lang.org/2020/09/25/rust-with-python.html)