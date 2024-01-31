---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
simple_title:         "Mettre une chaîne de caractères en majuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Mettre une chaîne de caractères en majuscules, c'est transformer toutes ses lettres en majuscules. Les développeurs font ça pour normaliser les données, pour l'importance visuelle, ou pour répondre à des besoins spécifiques de mise en forme.

## Comment faire :
```rust
fn main() {
    let salutation = "bonjour tout le monde";
    println!("{}", salutation.to_uppercase());
}

// Sortie :
// BONJOUR TOUT LE MONDE
```

## Plongée en profondeur :
Autrefois, la capitalisation était surtout manuelle : chaque lettre était vérifiée et transformée. En Rust, la méthode `.to_uppercase()` fait le travail pour nous. C’est plus complexe qu'une simple correspondance 'a' à 'A', particulièrement avec des caractères Unicode. Certains alphabets, comme le grec ou le russe, ont des règles uniques pour la capitalisation. Rust gère ça correctement grâce à sa prise en charge d’Unicode.

Alternatives ? Utilisez `.to_lowercase()` pour la transformation inverse ou `.capitalize()` dans certains crates pour ne capitaliser que la première lettre. 

Implémentation ? Rust itère sur chaque caractère Unicode, vérifie si une version en majuscules existe et la remplace si nécessaire. Performance-wise, attention aux gros textes : toute la chaîne est copiée.

## Voir aussi :
- Rust documentation on `.to_uppercase()`: [https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- Unicode `case mapping`: [https://www.unicode.org/reports/tr21/tr21-5.html](https://www.unicode.org/reports/tr21/tr21-5.html)
