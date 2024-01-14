---
title:    "Rust: Chaînes de concaténation"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi Concaténer des Chaînes de Caractères en Rust?

Les chaînes de caractères ou strings sont un élément fondamental dans la programmation. Elles représentent une séquence de caractères et sont utilisées pour stocker et manipuler du texte. L'une des opérations les plus courantes lors de la manipulation de strings est la concaténation, c'est-à-dire la fusion de deux ou plusieurs strings pour en former une plus grande. Dans cet article, nous verrons pourquoi il est important de comprendre comment concaténer des chaînes de caractères en Rust.

## Comment le Faire en Rust

Rust est un langage de programmation conçu pour être performant, sûr et concurrent. L'une des fonctionnalités les plus intéressantes de Rust est son système de types fort qui permet une gestion efficace de la mémoire. Pour concaténer des chaînes de caractères en Rust, nous pouvons utiliser l'opérateur `+` ou la macro `format!()`.

Voici un exemple utilisant l'opérateur `+` :

```Rust
let string1 = "Hello ";
let string2 = "world!";
let string3 = string1 + string2;

println!("{}", string3); // Output: Hello world!
```

La macro `format!()` permet également de concaténer des chaînes de caractères, mais avec une syntaxe plus conviviale :

```Rust
let string1 = "Hello";
let string2 = "world!";
let string3 = format!("{} {}", string1, string2);

println!("{}", string3); // Output: Hello world!
```

Il est important de noter que la macro `format!()` renvoie une `String`, tandis que l'opérateur `+` modifie les paramètres existants.

## Plongez Plus Profondément

En Rust, les chaînes de caractères sont représentées par le type `String` et par le type primitif `&str`. Ces deux types sont différents et ne peuvent pas être concaténés directement. Cependant, l'opérateur `+` peut être utilisé pour concaténer une `String` et une `&str`.

De plus, il existe une troisième option pour concaténer des chaînes de caractères en Rust : la méthode `push_str()` qui modifie une `String` en ajoutant une `&str` à la fin.

Voici un exemple utilisant la méthode `push_str()` :

```Rust
let mut string1 = String::from("Hello");
let string2 = "world!";
string1.push_str(" ");
string1.push_str(string2);

println!("{}", string1); // Output: Hello world!
```

## Voir Aussi

- [La documentation officielle sur les chaînes de caractères en Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Le tutoriel sur les chaînes de caractères en Rust de The Rust Programming Language](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Un article sur les types de données en Rust](https://www.freecodecamp.org/news/rust-data-types/)