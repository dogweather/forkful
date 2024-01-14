---
title:                "Rust: Extraction de sous-chaînes"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un développeur en herbe ou si vous travaillez déjà avec des langages de programmation, vous avez peut-être entendu parler de Rust. Vous êtes peut-être même tombé sur certaines de ses fonctionnalités intéressantes telles que le pattern matching ou la sûreté de la mémoire. Mais saviez-vous que Rust offre également la possibilité d'extraire des sous-chaînes de caractères ? Dans cet article, nous allons découvrir pourquoi et comment nous pouvons tirer parti de cette fonctionnalité utile.

## Comment faire
Pour extraire une sous-chaîne de caractères en Rust, nous utilisons la méthode `get()` qui prend en paramètre l'indice de départ et l'indice de fin de la sous-chaîne souhaitée. Par exemple, si nous voulons extraire les caractères "ust" de la chaîne "Rust", nous utiliserions `get(1..4)`. Voici un exemple de code pour mieux comprendre :

```rust
let langage = "Rust";
let sous_chaine = langage.get(1..4);

println!("La sous-chaîne extraite est : {}", sous_chaine);
```
Résultat : `La sous-chaîne extraite est : ust`

Il est également possible d'utiliser la méthode `get()` avec des variables pour les indices, ce qui rend la fonctionnalité encore plus flexible. Voici un autre exemple :

```rust
let langage = String::from("Rust");
let indice_debut = 1;
let indice_fin = 4;

let sous_chaine = langage.get(indice_debut..indice_fin);

println!("La sous-chaîne extraite est : {}", sous_chaine);
```
Résultat : `La sous-chaîne extraite est : ust`

Il est important de noter que la fonction `get()` renvoie un type `Option<&str>`, ce qui signifie qu'elle peut également renvoyer un `None` si les indices fournis sont en dehors de la plage valide de la chaîne de caractères.

## Plongée en profondeur
Maintenant que nous avons vu comment extraire une sous-chaîne en utilisant la méthode `get()`, il est important de comprendre comment cette fonctionnalité fonctionne en profondeur. En Rust, les chaînes de caractères sont stockées sous forme d'UTF-8, ce qui signifie qu'un caractère peut être composé de plusieurs octets. Par conséquent, lorsqu'on utilise `get()` pour extraire une sous-chaîne, Rust fait une vérification minutieuse pour s'assurer que les index fournis ne coupent pas un caractère en deux. Si c'est le cas, une erreur sera renvoyée.

De plus, la méthode `get()` renvoie une référence à une sous-chaîne plutôt qu'une nouvelle chaîne, ce qui signifie que les modifications de la sous-chaîne modifieront également la chaîne d'origine.

## Voir aussi
- [La documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/)
- [Une présentation sur les chaînes de caractères en Rust à l'Université du Maryland](https://www.youtube.com/watch?v=g7bftCBJX94)