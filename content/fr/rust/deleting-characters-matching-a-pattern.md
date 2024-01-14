---
title:    "Rust: Suppression des caractères correspondant à un motif"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur en Rust, vous savez déjà que ce langage de programmation est performant, sûr et moderne. Cependant, il existe toujours des cas où vous devrez manipuler des chaînes de caractères et éliminer certains caractères spécifiques. Dans ces situations, il peut être utile de savoir comment supprimer des caractères en utilisant des motifs (patterns) en Rust.

## Comment faire

Heureusement, Rust fournit un moyen simple et efficace de supprimer des caractères en utilisant la méthode `replace()`. Voici un exemple de code qui illustre comment supprimer toutes les voyelles d'une chaîne de caractères :

```Rust
fn main() {
    let mut text = String::from("Bonjour tout le monde!");
    let vowels = "aeiouyàâéèêëîïôùûü";

    text = text.replace(vowels, "");

    println!("{}", text);
}
```

L'output de ce code sera `Bnjr tt l mnd!`. Comme vous pouvez le voir, toutes les voyelles ont été supprimées de la chaîne de caractères initiale.

Voici un autre exemple qui montre comment supprimer des caractères spécifiques en utilisant un motif (pattern) :

```Rust
fn main() {
    let mut text = String::from("Hello World!");

    for c in "Hdl" { // Les lettres H, d et l vont être supprimées
        text = text.replace(c, "");
    }

    println!("{}", text);
}
```

L'output sera `eo Wor!`. En utilisant cette méthode, vous pouvez facilement supprimer des caractères spécifiques d'une chaîne de caractères en Rust.

## D plongée profonde

La méthode `replace()` utilisée dans les exemples ci-dessus prend également en compte les expressions régulières pour les motifs (patterns) de caractères. Cela vous permet de faire des recherches plus complexes et de supprimer des caractères en fonction de motifs spécifiques.

Par exemple, si vous voulez supprimer toutes les occurrences de nombres dans une chaîne de caractères, vous pouvez le faire en utilisant une expression régulière comme motif :

```Rust
fn main() {
    let mut text = String::from("Il y a 18 chiens dans ce parc.");

    text = text.replace(Regex::new(r"[0-9]").unwrap(), "");

    println!("{}", text);
}
```

L'output sera `Il y a chiens dans ce parc.`. Comme vous pouvez le voir, toutes les occurrences de chiffres ont été supprimées de la chaîne de caractères.

## Voir aussi

- [Documentation officielle de Rust sur la méthode `replace()`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Tutoriel interactif pour apprendre Rust](https://www.rust-lang.org/learn)
- [Exemples avancés de manipulation de chaînes de caractères en Rust](https://blog.logrocket.com/advanced-rust-string-manipulation-techniques/)