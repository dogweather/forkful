---
title:    "Rust: Génération de nombres aléatoires"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur passionné par le langage Rust, vous savez probablement déjà toute l'efficacité et la performance qu'il peut offrir. Mais saviez-vous qu'il est également très utile pour générer des nombres aléatoires ? Dans cet article, nous allons vous expliquer pourquoi il peut être intéressant pour vous d'explorer cette fonctionnalité.

## Comment faire

Pour générer des nombres aléatoires en Rust, nous pouvons utiliser la bibliothèque standard rand. Voici un exemple de code qui génère un nombre aléatoire entre 1 et 100 et l'affiche à l'écran :

```Rust
use rand::Rng;
let random_number = rand::thread_rng().gen_range(1, 101);
println!("Le nombre aléatoire est : {}", random_number);
```

Voici un exemple de sortie possible :
```
Le nombre aléatoire est : 57
```
Comme vous pouvez le constater, le code est simple et facile à comprendre. Mais nous pouvons également personnaliser la génération de nombres aléatoires en définissant des limites et des distributions spécifiques. Pour en savoir plus, consultez la documentation de la bibliothèque rand.

## Plongez plus profondément

Maintenant que vous savez comment générer des nombres aléatoires en Rust, vous pouvez vous demander comment cela fonctionne réellement. En fait, le générateur de nombres aléatoires de la bibliothèque rand utilise un algorithme appelé Xorshift pour produire des nombres pseudo-aléatoires. Cela signifie que les nombres générés semblent aléatoires, mais en réalité, ils suivent une séquence prédéfinie.

Si vous voulez en savoir plus sur le fonctionnement de cet algorithme, nous vous recommandons de consulter les sources de la bibliothèque rand et d'autres ressources sur le web.

## Voir aussi

- [Documentation de la bibliothèque rand](https://docs.rs/rand)
- [Article sur les générateurs de nombres aléatoires en Rust](https://dev.to/uinstinct/generating-random-numbers-in-rust-57hg)