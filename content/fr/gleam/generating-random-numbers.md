---
title:    "Gleam: Génération de nombres aléatoires"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une tâche fréquente dans la programmation, que ce soit pour des simulations, des jeux ou pour tout autre usage qui nécessite de l'aléatoire. Avec Gleam, il est facile de générer des nombres aléatoires avec une grande précision et de contrôler la distribution de ces nombres.

## Comment Faire

Pour générer des nombres aléatoires en Gleam, nous pouvons utiliser la fonction `random.float` pour générer un nombre aléatoire entre 0 et 1. Vous pouvez également spécifier une plage de valeurs en utilisant les paramètres `min` et `max`. Voici un exemple de code:

```
Gleam import Random

fn main() {
    let random_number = Random.float(min=1.0, max=10.0)
    io.println("Nombre Aléatoire: " + random_number)
}
```

Lorsque vous executez ce code, vous obtiendrez un nombre aléatoire compris entre 1 et 10, comme par exemple: "Nombre Aléatoire: 7.532".

Il est également possible de générer des nombres aléatoires entiers en utilisant la fonction `random.int`. Cette fonction prend en compte les paramètres `min` et `max` pour définir la plage de valeurs possibles. Voici un exemple de code:

```
Gleam import Random

fn main() {
    let random_number = Random.int(min=1, max=10)
    io.println("Nombre Aléatoire: " + random_number)
}
```

Ce code générera un nombre entier aléatoire entre 1 et 10, comme par exemple: "Nombre Aléatoire: 8".

Il est également important de noter que Gleam utilise un générateur de nombres aléatoires cryptographiquement sûr, garantissant une distribution équilibrée et prévisible des nombres générés.

## Plongée Profonde

Saviez-vous que la génération de nombres aléatoires n'est pas réellement aléatoire, mais plutôt basée sur des algorithmes déterministes? Pour cette raison, il est important d'utiliser un générateur de nombres aléatoires de qualité pour éviter tout biais ou prévisibilité.

Gleam utilise l'algorithme Xorshift pour générer des nombres aléatoires, qui est connu pour sa rapidité et sa bonne qualité de distribution des valeurs. Cet algorithme utilise un état interne pour déterminer le prochain nombre généré, ce qui le rend très efficace.

## Voir Aussi

- [Documentation sur la fonction `random` dans Gleam](https://gleam.run/packages/random/)
- [Article sur l'algorithme Xorshift pour les nombres aléatoires](https://www.encyclopediaofmath.org/index.php/Xorshift_generator)
- [Article sur la génération de nombres aléatoires de qualité dans les jeux vidéo](https://www.gamasutra.com/blogs/RobertDowling/20190814/346156/Generating_HighQuality_Random_Numbers_in_Gameplay.php)
- [Exemple d'utilisation de nombres aléatoires dans un jeu en Gleam](https://github.com/smashed-avo/gale)