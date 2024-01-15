---
title:                "Conversion d'une chaîne en minuscule"
html_title:           "Rust: Conversion d'une chaîne en minuscule"
simple_title:         "Conversion d'une chaîne en minuscule"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Rust, vous avez probablement rencontré des situations où vous souhaitiez modifier la casse d'une chaîne pour des raisons de traitement ou d'affichage. Heureusement, Rust offre une fonctionnalité intégrée pour convertir une chaîne en minuscules, ce qui vous permet de le faire facilement et efficacement.

## Comment Faire

Pour convertir une chaîne en minuscules en Rust, vous pouvez utiliser la méthode "to_lowercase" disponible pour les chaînes de caractères. Voici un exemple de code :

```Rust
let my_string = String::from("Bienvenue en Rust !");
let lowercase_string = my_string.to_lowercase();
println!("La chaîne en minuscules est : {}", lowercase_string);
```

La sortie de ce code sera :

```text
La chaîne en minuscules est : bienvenue en rust !
```

Comme vous pouvez le voir, la méthode "to_lowercase" a transformé toutes les lettres en minuscules, y compris les caractères spéciaux.

Si vous souhaitez modifier la casse uniquement pour une partie de la chaîne, vous pouvez utiliser la méthode "to_lowercase" sur un tronçon de la chaîne. Voici un exemple :

```Rust
let my_string = String::from("La vie est belle");
let first_word = &my_string[0..2];
let lowercase_first_word = first_word.to_lowercase();
println!("La première lettre en minuscules est : {}", lowercase_first_word);
```

La sortie de ce code sera :

```text
La première lettre en minuscules est : la
```

La méthode "to_lowercase" peut également être utilisée avec des chaînes de caractères multi-octets, ce qui permet de gérer les caractères Unicode. Par exemple :

```Rust
let my_string = String::from("大家好！");
let lowercase_string = my_string.to_lowercase();
println!("La chaîne en minuscules est : {}", lowercase_string);
```

La sortie de ce code sera :

```text
La chaîne en minuscules est : 大家好！
```

Vous pouvez également utiliser la méthode "to_lowercase" pour convertir une chaîne en minuscules sans en créer une autre. Voici un exemple :

```Rust
let mut my_string = String::from("Bonjour !");
my_string.make_ascii_lowercase();
println!("La chaîne en minuscules est : {}", my_string);
```

La sortie de ce code sera :

```text
La chaîne en minuscules est : bonjour !
```

## Plongée Profonde

Le processus de conversion d'une chaîne en minuscules en Rust peut sembler simple, mais il y a quelques choses à garder à l'esprit.

Tout d'abord, la méthode "to_lowercase" utilise la norme Unicode pour effectuer la conversion, ce qui signifie qu'elle peut gérer les caractères de toutes les langues et cultures. Cependant, cela peut également entraîner des différences de casse entre les différentes normes utilisées dans différents systèmes d'exploitation.

Deuxièmement, la méthode "to_lowercase" ne modifie pas la chaîne d'origine, mais renvoie plutôt une nouvelle chaîne en minuscules. Si vous voulez changer la casse de la chaîne d'origine, vous devez utiliser la méthode "make_ascii_lowercase" ou d'autres méthodes de modification de la chaîne.

Enfin, si vous avez besoin de performances optimales lors de la conversion de chaînes en minuscules, vous pouvez utiliser la bibliothèque de données "unicode-normalization" qui offre des fonctionnalités avancées pour le traitement de chaînes Unicode.

## Voir aussi

- [Documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/)
- [Guide de Rust pour les chaînes de caractères et les interactions avec Unicode](https://blog.thoughtram.io/string-processing-in-rust/)
- [La bibliothèque unicode-normalization pour le traitement avancé des chaînes en Rust](https://github.com/unicode-rs/unicode-normalization)