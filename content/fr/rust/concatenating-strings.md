---
title:                "Rust: Concaténation de chaînes de caractères"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

Les chaînes de caractères sont un élément essentiel de tout programme. Elles nous permettent de stocker et de manipuler du texte de manière efficace. Dans certains cas, nous pouvons avoir besoin de fusionner plusieurs chaînes de caractères pour en créer une nouvelle. C'est là qu'intervient la concaténation de chaînes. Vous apprendrez dans cet article pourquoi et comment utiliser la concaténation de chaînes en Rust.

## Comment faire

Pour concaténer des chaînes en Rust, nous allons utiliser l'opérateur `+` ou la méthode `format!()`. Voyons un exemple avec l'opérateur `+` :

```Rust
let nom = "Jean";
let nom_complet = nom + " Dupont";

println!("{}", nom_complet); // affiche "Jean Dupont"
```

Ici, nous avons créé une nouvelle chaîne en ajoutant le texte " Dupont" à la fin du prénom "Jean". Notez que l'opérateur `+` a fusionné les deux chaînes ensemble sans ajouter d'espace entre elles. Si vous souhaitez ajouter un espace, vous pouvez utiliser l'opérateur `+` avec une chaîne contenant un espace :

```Rust
let nom = "Jean";
let nom_complet = nom + " " + "Dupont";

println!("{}", nom_complet); // affiche "Jean Dupont"
```

Une autre façon de concaténer des chaînes en Rust est d'utiliser la méthode `format!()`. Cela nous permet d'utiliser des variables et de formater la chaîne comme nous le souhaitons :

```Rust
let nom = "Jean";
let nom_de_famille = "Dupont";

let nom_complet = format!("{} {}", nom, nom_de_famille);

println!("{}", nom_complet); // affiche "Jean Dupont"
```

## Plongée en profondeur

La concaténation de chaînes peut sembler simple, mais il y a quelques points à garder à l'esprit lorsque vous l'utilisez en Rust. Tout d'abord, il est important de comprendre que la concaténation de chaînes peut être coûteuse en termes de performances. Cela est dû au fait que chaque fois que nous concaténons des chaînes, une nouvelle chaîne est créée en mémoire. Si vous devez concaténer un grand nombre de chaînes, il est recommandé d'utiliser la méthode `format!()` qui est plus efficace en termes de performances.

En outre, en Rust, la concaténation de chaînes peut être un peu délicate lorsque vous travaillez avec des types de données non primitifs tels que `String` ou `str`. En effet, la concaténation peut entraîner la consommation inutile de mémoire ou des erreurs lors de la compilation. Pour éviter cela, il est recommandé d'utiliser des types de données tels que `String` et `str` avec précaution et de comprendre comment ils sont gérés par le langage.

## Voir aussi

- [Guide officiel de Rust sur les chaînes](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Documentation officielle de la méthode `format!()`](https://doc.rust-lang.org/std/macro.format.html)
- [Blog sur les performances de la concaténation de chaînes en Rust](https://deterministic.space/string-fuckery.html)