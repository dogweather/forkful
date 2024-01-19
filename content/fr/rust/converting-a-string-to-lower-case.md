---
title:                "Convertir une chaîne en minuscules"
html_title:           "PHP: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Convertissez une chaîne en minuscules est un processus pour changer toutes les lettres majuscules d'une chaîne de caractères en lettres minuscules. Les programmeurs font cela pour normaliser les données, ce qui est essentiel lors de la comparaison, de la recherche et du classement des chaînes.

## Comment faire:

Voici comment vous pouvez convertir une chaîne en minuscules dans Rust.

```Rust
// déclaration et assignation d'une chaîne de caractères
let ma_chaine = "HELLO, WORLD!";
// conversion en minuscules
let en_minuscules = ma_chaine.to_lowercase();
// imprimer la chaîne en minuscules
println!("{}", en_minuscules);
```

Cela va produire:

```Rust
"hello, world!"
```

## Approfondissement

1. Contexte historique: Travailler avec des chaînes de caractères a toujours été une partie intégrante de la programmation. La conversion en minuscules a été conçue pour faciliter les opérations insensibles à la casse.
2. Alternatives: Si vous ne voulez pas convertir toute la chaîne en minuscules, vous pouvez utiliser la fonction `chars` pour obtenir un itérateur sur les caractères, puis utiliser la méthode `to_lowercase` sur chaque caractère.
3. Détails de mise en œuvre: La méthode to_lowercase de Rust fonctionne bien pour les lettres ASCII, mais elle est aussi capable de convertir les caractères Unicode. C'est important car toutes les langues n'utilisent pas l'ASCII.

## À voir aussi:

Les liens suivants sont de bonnes ressources pour en savoir plus sur les chaînes et les caractères dans Rust:

- Documentation Rust sur les méthodes de caractères de chaîne: [https://doc.rust-lang.org/std/string/struct.String.html](https://doc.rust-lang.org/std/string/struct.String.html)
- Le livre de programmation Rust: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Unicode et Rust: [https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html](https://www.ameyalokare.com/rust/2017/10/12/rust-str-vs-String.html)