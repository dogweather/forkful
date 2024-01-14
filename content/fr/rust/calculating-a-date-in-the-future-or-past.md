---
title:    "Rust: Calculer une date dans le futur ou dans le passé"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#
## Pourquoi

Le calcul de dates dans le passé ou dans le futur peut être utile dans de nombreux cas, notamment pour planifier des événements, effectuer des calculs financiers ou créer des rappels. En utilisant Rust, un langage de programmation moderne et performant, vous pouvez facilement implémenter cet algorithme de calcul de date.

## Comment faire

Pour commencer, vous devez d'abord importer la bibliothèque standard de Rust qui contient toutes les fonctions nécessaires pour travailler avec des dates. Dans votre fichier source, ajoutez l'instruction `use std::time::Instant;` en haut du fichier.

Ensuite, vous pouvez utiliser l'utilisation suivante pour calculer une date dans le futur ou dans le passé:

```Rust
let now = Instant::now(); // Initialise une date courante
let annee = now.year() + 1; // Additionne une année à la date courante
let mois = now.month() - 1; // Soustrait un mois à la date courante
let jour = now.day(); // Garde le jour de la date courante
```

Dans l'exemple ci-dessus, nous avons utilisé les fonctions `year()`, `month()` et `day()` pour extraire respectivement l'année, le mois et le jour de la date courante. Ensuite, nous avons ajouté ou soustrait un nombre à ces valeurs pour calculer une nouvelle date.

Notez que les fonctions `year()`, `month()` et `day()` renvoient des valeurs `u32` (entiers non signés de 32 bits). Si vous souhaitez utiliser des valeurs de date différentes, telles que des dates avec des heures et des minutes précises, vous pouvez utiliser la fonction `now()` qui renvoie un `Instant` complet à partir duquel vous pouvez extraire toutes les informations de date et d'heure.

## Plongée en profondeur

Le calcul de dates dans le futur ou dans le passé peut sembler simple au premier abord, mais il convient de noter qu'il existe des problèmes complexes associés à la gestion de différents calendriers et fuseaux horaires. Cependant, Rust dispose de nombreuses bibliothèques tierces pour gérer ces problèmes, telles que `chrono`, `time` ou `dates`, qui fournissent des fonctionnalités avancées pour travailler avec des dates.

De plus, la bibliothèque standard de Rust dispose également de plusieurs fonctions pour vérifier la validité d'une date, la convertir en une chaîne de caractères et effectuer d'autres opérations courantes.

## Voir aussi

- https://doc.rust-lang.org/std/time/struct.Instant.html
- https://doc.rust-lang.org/std/time/index.html
- https://crates.io/crates/chrono
- https://crates.io/crates/time
- https://crates.io/crates/dates