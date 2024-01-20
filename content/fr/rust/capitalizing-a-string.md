---
title:                "Mettre en majuscules une chaîne de caractères"
html_title:           "Rust: Mettre en majuscules une chaîne de caractères"
simple_title:         "Mettre en majuscules une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Article sur la Programmation Rust: Comment Capitaliser une Chaîne de Caractères

## Quoi & Pourquoi ?
Capitaliser une chaîne, c'est transformer la première lettre de chaque mot en majuscule. Les programmeurs le font pour améliorer la lisibilité des textes ou pour respecter certaines normes formatives.

## Comment faire:
Voici comment on peut capitaliser une chaîne en Rust.

```Rust
// On utilise la fonction 'to_uppercase' de 'char' pour capitaliser
fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(first) => first.to_uppercase() + c.as_str(),
    }
}

fn main() {
    let s = "bonjour, monde!";
    println!("{}", capitalize(s));  // Affiche: "Bonjour, monde!"
}
```

## Plongée Profonde

**Contexte Historique:** Rust a repris cette fonctionnalité de nombreux autres langages de programmation, comme Java et Python, qui ont tous des fonctions similaires pour capitaliser des chaînes.

**Alternatives:** Vous pourriez aussi utiliser la bibliothèque `unicase` qui fournit une opération de capitalisation plus sophistiquée, en tenant compte des cas spéciaux dans différentes langues.

**Détails d'implémentation:** En Rust, les caractères d'une chaîne sont stockés sous forme de graphèmes. Quand nous appelons `to_uppercase()` sur le premier caractère, Rust parcourt chaque point de code du graphème et les convertit en majuscules si possible.

## Voir Aussi

Pour plus d'informations sur les chaînes de caractères dans Rust, consultez la documentation officielle sur les String [ici](https://doc.rust-lang.org/std/string/). Pour en savoir plus sur les méthodes de `char`, consultez [ce lien](https://doc.rust-lang.org/std/primitive.char.html). 

Et si vous êtes intéressé par les détails de l'internationalisation, lisez à propos de 'unicode-segmentation', une bibliothèque externe que Rust utilise pour diviser les chaînes en graphèmes [ici](https://docs.rs/unicode-segmentation/1.2.1/unicode_segmentation/).