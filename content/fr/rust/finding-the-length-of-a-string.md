---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trouver la Longueur d'une Chaîne en Rust

## C'est quoi & Pourquoi?
Trouver la longueur d'une chaîne signifie déterminer le nombre de caractères dans une séquence particulière de texte. C'est une tâche récurrente en programmation, utilisée pour manipuler et gérer efficacement les données textuelles.

## Comment faire:
Rust facilite le calcul de la longueur d'une chaîne avec sa méthode intégrée `.len()`. Voyez par vous-même:

```rus
fn main() {
    let chaine = "Bonjour le monde!";
    println!("La longueur est : {}", chaine.len());
}
```

En exécutant ce script, vous verrez que l'output de la ligne `println!` est `18`. Le script compte bien chaque caractère, y compris les espaces.

## Plongée Profonde
Historiquement, déterminer la longueur d'une chaîne en Rust n'a pas toujours été aussi simple qu'aujourd'hui. Les premières versions de Rust utilisaient la fonction `str::len()` plutôt que la méthode `.len()`. Avec l'évolution du langage, la méthode `.len()` a été introduite pour une utilisation plus ergonomique.

Il convient également de mentionner que la méthode `.len()` retourne le nombre de bytes dans une chaîne, plutôt que le nombre de caractères Unicode. Si vous avez besoin de compter les caractères Unicode dans une chaîne, vous pouvez utiliser la méthode `.chars().count()`.

```rust
fn main() {
    let chaine = "中文字符";
    println!("La longueur est : {}", chaine.len());  // compte le nombre de bytes
    println!("Le nombre de caractères est : {}", chaine.chars().count());  // compte le nombre de caractères
}
```

Dans cet exemple, `chaine.len()` retourne `12` parce qu'il y a 12 bytes en tout (chaque caractère Unicode en Mandarin prend 3 bytes), alors que `chaine.chars().count()` retourne `4`, qui est le nombre de caractères Unicode.

## Voir Aussi
Voici quelques ressources supplémentaires qui peuvent vous aider à comprendre comment trouver la longueur d'une chaîne en Rust:

- Documentation Rust sur les chaînes : https://doc.rust-lang.org/stable/book/ch08-02-strings.html
- Stackoverflow: Comment obtenir la longueur d'une chaîne en Rust : https://stackoverflow.com/questions/26990453/how-do-i-get-the-length-of-a-string-in-rust
- Guide Rust sur les méthodes de chaînes: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-string-methods.html

Et voilà! Maintenant, vous savez comment trouver la longueur d'une chaîne en Rust!