---
title:                "Rust: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion de chaînes en lettres minuscules peut sembler simple, mais elle peut avoir un impact significatif sur la lisibilité et la cohérence de votre code. En utilisant Rust, nous pouvons facilement convertir toute chaîne de caractères en lettres minuscules, ce qui rend notre code plus lisible et plus facile à maintenir.

## Comment faire

```Rust
let mut word = String::from("HELLO");
word.to_lowercase();
```

La méthode `to_lowercase()` s'applique à un type de chaîne de caractères et renvoie une nouvelle chaîne avec toutes les lettres converties en minuscules. Elle est très facile à utiliser et ne nécessite pas d'importer de bibliothèques externes.

```Rust
// Entrée: "HeLlO WoRlD"
// Sortie: "hello world"
```

## Approfondissement

La conversion de chaînes en minuscules utilise le concept de propriété de propriété en Rust. En utilisant le signe `mut`, nous pouvons rendre la chaîne mutable, ce qui signifie qu'elle peut être modifiée. La méthode `to_lowercase()` modifie directement la chaîne d'origine et renvoie une nouvelle chaîne avec des lettres minuscules. Si la chaîne n'est pas déclarée comme mutable, l'appel à `to_lowercase()` échouera car elle ne peut pas modifier la valeur d'une chaîne constante.

De plus, cette méthode de conversion de chaînes utilise également la capacité de Rust à gérer les caractères Unicode. Cela signifie que les caractères spéciaux et les lettres accentuées seront également convertis en minuscules de manière cohérente, ce qui peut être un avantage par rapport à d'autres langages de programmation.

## Voir aussi

- [Documentation Rust sur la conversion de chaînes en lettres minuscules](https://doc.rust-lang.org/rand/std/vec/struct.Vec.html#method.to_lowercase)
- [Article sur l'utilisation de chaînes en lettres minuscules pour améliorer la lisibilité du code en Rust](https://medium.com/@younesedd11/how-to-use-string-lowercase-to-improve-readability-in-rust-5a4c4727d8c0)
- [Exemples de code de conversion de chaînes en lettres minuscules en Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1f7a93b5b5e3a5dc9642c651e1cc9423)