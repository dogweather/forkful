---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "Rust: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est de gérer et de manipuler des chaînes de caractères. Il peut arriver qu'on ait besoin de supprimer des caractères qui correspondent à un certain modèle, qu'il s'agisse de caractères spéciaux, d'espaces ou de chiffres. Dans cet article, nous allons voir comment le faire en utilisant Rust, un langage de programmation moderne et performant.

## Comment faire

La façon la plus simple et la plus efficace de supprimer des caractères correspondant à un modèle est d'utiliser la fonction `replace` du type `String` en combinaison avec des expressions régulières. Voici un exemple de code en Rust:

```
let chaine = "Hello, 1234!".to_string();
let chaine_modifiee = chaine.replace(|c: char| c.is_numeric(), "");
println!("{}", chaine_modifiee);
```

Dans cet exemple, nous avons une chaîne de caractères qui contient des lettres et des chiffres. Nous utilisons ensuite la méthode `replace` pour remplacer tous les caractères numériques par une chaîne vide. Ainsi, la variable `chaine_modifiee` contiendra désormais "Hello, !".

L'utilisation de la fonction `replace` avec une expression régulière nous permet de spécifier un modèle plus complexe pour les caractères à supprimer. Par exemple, si nous voulons supprimer tous les caractères spéciaux d'une chaîne, nous pouvons utiliser l'expression régulière `[^a-zA-Z0-9]` qui correspond à tout caractère qui n'est ni une lettre ni un chiffre. Voici un exemple de code utilisant cette expression régulière:

```
let chaine = "Hello, @#$!".to_string();
let chaine_modifiee = chaine.replace(regex::Regex::new(r"[^a-zA-Z0-9]").unwrap(), "");
println!("{}", chaine_modifiee);
```

Dans ce cas, la variable `chaine_modifiee` contiendra "Hello". Nous utilisons ici la bibliothèque `regex` pour créer notre expression régulière, mais vous pouvez également utiliser celle intégrée à Rust en utilisant le `mod regex`.

## Plonger plus en profondeur

En regardant de plus près la fonction `replace`, on peut voir qu'elle prend en paramètre une closure ou une expression régulière. La closure est une fonction anonyme qui peut être utilisée pour spécifier un modèle personnalisé. Pour un exemple plus avancé, on pourrait vouloir supprimer tous les caractères de ponctuation d'une chaîne en utilisant une closure:

```
let ponctuation = ['!', '"', '(', ')', '-', '*', '.', '⸱'];
let chaine = "Hello, world!".to_string();
let chaine_modifiee = chaine.replace(|c: char| { ponctuation.contains(&c) }, "");
println!("{}", chaine_modifiee);
```

Dans cet exemple, nous créons d'abord un tableau contenant tous les caractères de ponctuation que nous voulons supprimer. Ensuite, dans la closure de la méthode `replace`, nous utilisons la méthode `contains` pour vérifier si le caractère actuel se trouve dans le tableau `ponctuation`. Si c'est le cas, la méthode `replace` le remplacera par une chaîne vide.

## Voir aussi

- [Documentation officielle de la méthode `replace` en Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Documentation de la bibliothèque `regex` pour Rust](https://docs.rs/regex/)
- [Guide de référence sur les expressions régulières en Rust](https://doc.rust-lang.org/std/regex/index.html)