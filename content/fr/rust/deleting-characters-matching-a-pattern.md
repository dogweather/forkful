---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Ruby: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer les caractères correspondant à un motif est une opération qui consiste à éliminer de façon sélective certains caractères dans une chaîne de texte. C'est une tâche courante en programmation pour éliminer les données inutiles ou pour formater les données en vue de leur analyse.

## Comment faire:

Voici comment vous pouvez supprimer certains caractères dans Rust. Disons que vous voulez supprimer tous les 'a' de la chaîne "banana". Vous pouvez le faire avec la méthode `replace()`:

```Rust
let chaine = "banana";
let nouveau = chaine.replace("a", "");
println!("{}", nouveau);
```

Output:

```bash
bnn
```

Comme vous pouvez le voir, tous les 'a' de "banana" ont été supprimés.

## Un plongeon plus profond:

Historiquement, la suppression de caractères en fonction d'un motif est un concept qui provient du langage de programmation Perl. Rust possède sa propre implémentation efficace basée sur ses mécanismes de gestion de la mémoire et de la chaîne.

Alternativement, on peut utiliser la méthode `chars().filter()`, qui donne plus de flexibilité en termes de modèles que vous pouvez supprimer, mais qui est généralement plus lente.

```Rust
let chaine = "banana";
let nouveau: String = chaine.chars().filter(|&c| c != 'a').collect();
println!("{}", nouveau);
```

Output:

```bash
bnn
```

En interne, la fonction `replace()` repose sur le concept de recherche et remplacement. Elle recherche le motif spécifié et le remplace par une chaîne vide. La performance de cette opération dépend du nombre d'occurrences du motif.

## Voir aussi:

Pour en savoir plus sur la programmation Rust et les chaînes en particulier, consultez les ressources suivantes:

- La documentation officielle de Rust sur les chaînes <https://doc.rust-lang.org/std/string/>
- Le livre de Rust, un guide complet de toute la programmation en Rust <https://doc.rust-lang.org/book/>
- Le forum des utilisateurs de Rust, où vous pouvez poser des questions et discuter des sujets liés à Rust <https://users.rust-lang.org/>