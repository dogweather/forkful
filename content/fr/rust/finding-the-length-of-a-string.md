---
title:    "Rust: Déterminer la longueur d'une chaîne de caractères"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation, et c'est également un bon moyen de se familiariser avec le langage Rust. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne de caractères en utilisant Rust.

## Comment Faire

Pour trouver la longueur d'une chaîne de caractères en Rust, nous pouvons utiliser la méthode `len()` de la structure `str`. Cette méthode renvoie un entier représentant le nombre de caractères dans la chaîne. Voici un exemple de code :

```Rust
let s = "Bonjour, le monde !";
println!("La longueur de la chaîne est de {} caractères.", s.len());
```

Output : La longueur de la chaîne est de 21 caractères.

Comme nous pouvons le voir dans l'exemple, nous utilisons la méthode `len()` sur la chaîne `s` pour obtenir sa longueur. Nous pouvons également utiliser `chars()` pour obtenir une itération sur les caractères de la chaîne, et ensuite utiliser `count()` pour obtenir le nombre de caractères :

```Rust
let s = "Bonjour, le monde !";
println!("La longueur de la chaîne est de {} caractères.", s.chars().count());
```

Output : La longueur de la chaîne est de 19 caractères.

## Deep Dive

En Rust, la longueur d'une chaîne de caractères est déterminée par le nombre de points de code Unicode dans la chaîne. Cela signifie que même si une chaîne peut sembler avoir le même nombre de caractères, sa longueur réelle peut varier en fonction des caractères spéciaux ou des accents qu'elle contient.

De plus, en utilisant la méthode `len()`, nous obtenons le nombre de points de code en tant qu'entier. Si nous voulons obtenir le nombre de bytes utilisés par la chaîne, nous pouvons utiliser la méthode `as_bytes().len()`, qui renvoie la taille en bytes de la chaîne. Cela peut être utile si nous devons prendre en compte la mémoire utilisée par la chaîne dans notre programme.

## Voir Aussi

Pour en savoir plus sur la manipulation des chaînes de caractères en Rust, vous pouvez consulter ces liens :

- [Guide officiel sur les chaînes de caractères en Rust](https://doc.rust-lang.org/std/string/index.html)
- [Exemples pratiques de manipulation de chaînes en Rust](https://docs.rs/crate/str_tools/0.1.0/source/examples.rs)
- [Comparaison de la performance de différentes méthodes pour trouver la longueur d'une chaîne en Rust](https://medium.com/sean3z/rust-strings-20851e70e84c)