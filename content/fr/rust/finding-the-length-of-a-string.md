---
title:                "Rust: Trouver la longueur d'une chaîne"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Dans le monde de la programmation, l'une des tâches les plus courantes est la manipulation de chaînes de caractères. Que vous soyez un débutant ou un développeur expérimenté, il est important de comprendre comment trouver la longueur d'une chaîne de caractères dans le langage de programmation que vous utilisez. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne de caractères en Rust et pourquoi il est important de maîtriser cette compétence.

## Comment faire 

La première chose à faire est de comprendre ce qu'est une chaîne de caractères en Rust. En Rust, une chaîne de caractères est une séquence de caractères UTF-8 encodés stockés dans le tas et représentés par le type `String`. Pour trouver la longueur d'une chaîne de caractères, nous pouvons utiliser la méthode `len()` qui renvoie le nombre de caractères dans la chaîne.

```Rust
let string = "Bonjour";
println!("La longueur de la chaîne de caractères est: {}", string.len());
```

La sortie de ce code sera:

```
La longueur de la chaîne de caractères est: 7
```

Nous pouvons également utiliser la méthode `chars()` pour obtenir un itérateur sur chaque caractère de la chaîne, puis compter le nombre d'itérations pour trouver la longueur de la chaîne.

```Rust
let string = "Bonjour";
let count = string.chars().count();
println!("La longueur de la chaîne de caractères est: {}", count);
```

La sortie de ce code sera la même que précédemment.

## Plongez plus profond 

Maintenant que nous savons comment trouver la longueur d'une chaîne de caractères en Rust, il est important de comprendre un peu plus en profondeur comment cela fonctionne. En utilisant la méthode `len()`, nous obtenons le nombre de caractères dans la chaîne, mais pourquoi est-ce important? En informatique, les chaînes de caractères sont souvent utilisées pour stocker des données textuelles. Si nous voulons manipuler ces données, il est essentiel de savoir exactement combien de caractères sont présents dans la chaîne. Par exemple, si nous voulons extraire un sous-ensemble de caractères de la chaîne, nous avons besoin de connaître la longueur de la chaîne pour déterminer où commencer et où s'arrêter.

Il est également important de noter que la méthode `len()` renvoie le nombre de caractères et non le nombre d'octets, car en Rust, une chaîne de caractères peut contenir des caractères multibytes qui occupent plus d'un octet.

## Voir aussi 

Maintenant que vous savez comment trouver la longueur d'une chaîne de caractères en Rust, voici quelques ressources utiles pour approfondir vos connaissances: 

- [Documentation officielle de Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/index.html)
- [Apprenez Rust en créant des projets réels](https://dev.to/codecrumbs/learn-rust-by-solving-real-problems-1bmo)
- [Chaînes de caractères en Rust: Heap vs Stack](https://dev.to/rohitrox/strings-in-rust-heap-vs-stack-54fi)