---
title:                "Rust: Suppression de caractères correspondant à un modèle"
simple_title:         "Suppression de caractères correspondant à un modèle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Le langage de programmation Rust est connu pour sa sûreté de mémoire et sa performance. Dans cet article, nous allons explorer comment utiliser Rust pour supprimer des caractères correspondant à un motif dans une chaîne de caractères. Cela peut être utile dans de nombreuses situations, comme le nettoyage de données ou la manipulation de chaînes de caractères.

## Comment faire

Pour supprimer des caractères correspondant à un motif dans une chaîne de caractères en Rust, nous allons utiliser la méthode ```.replace()```. Cette méthode prend en paramètre le motif à remplacer et le nouveau motif souhaité. Ensuite, nous pouvons utiliser la méthode ```.trim()``` pour supprimer les caractères supplémentaires, si nécessaire.

Voici un exemple de code pour supprimer les lettres "a" dans une chaîne de caractères :

```rust
let my_string = "La pluie en Espagne tombe principalement sur la côte";
let new_string = my_string.replace("a", "");
println!("{}", new_string); //Sortie : L pluie en Espgne tombe principlement sur la côte
```

Nous pouvons également spécifier plusieurs motifs à remplacer en utilisant une boucle ```for``` :

```rust
let mut my_string = "Je suis votre capitaine";
let patterns = ["e", "u"];
for pattern in &patterns {
    my_string = my_string.replace(pattern, "");
}
println!("{}", my_string); //Sortie : J suis votr capitain
```

Si nous voulons également supprimer les espaces autour des caractères correspondants, nous pouvons utiliser la méthode ```.trim()``` de la manière suivante :

```rust
let my_string = "____Heureusement, nous sommes ensemble !___";
let new_string = my_string.replace("_", " ").trim();
println!("{}", new_string); //Sortie : Heureusement, nous sommes ensemble !
```

## Plongée en profondeur

La méthode ```.replace()``` utilise des expressions régulières pour trouver et remplacer les motifs dans une chaîne de caractères. Cela signifie que nous pouvons utiliser des expressions régulières plus complexes pour cibler des patterns spécifiques. Par exemple, si nous voulons supprimer tous les caractères non alphabétiques dans une chaîne, nous pouvons utiliser l'expression régulière ```[^a-zA-Z]```.

Nous pouvons également utiliser la méthode ```.replace()``` pour supprimer des motifs de sous-chaînes spécifiques, en utilisant les index des caractères. Par exemple, si nous voulons supprimer les deux derniers caractères d'une chaîne, nous pouvons utiliser l'expression régulière ```\S{2}$```.

## Voir aussi
- [Documentation officielle de Rust pour la méthode .replace()](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Documentation officielle de Rust pour la méthode .trim()](https://doc.rust-lang.org/std/string/struct.String.html#method.trim)
- [Tutorial sur les expressions régulières en Rust](https://freesewing.dev/tutorials/rust/regexp/)
- [Exemples d'expressions régulières pour Rust](https://doc.rust-lang.org/1.0.0/regex/regex/index.html)