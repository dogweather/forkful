---
title:                "Rust: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi extrait-on des sous-chaînes en Rust ?

L'une des fonctionnalités les plus utiles de la programmation est la capacité à manipuler et extraire des parties spécifiques de données au sein d'une chaîne de caractères. En Rust, cela peut être réalisé à l'aide de la méthode `split()` ou de la fonction `slice()` pour extraire des sous-chaînes. Dans cet article, nous allons découvrir pourquoi et comment utiliser ces méthodes en Rust.

## Comment faire

La première étape pour extraire des sous-chaînes en Rust est d'utiliser la fonction `slice()`. Cette fonction prend en paramètre la chaîne de caractères d'origine ainsi que l'indice de début et l'indice de fin de la sous-chaîne que nous souhaitons extraire. Par exemple, si nous avons une chaîne de caractères `"Bonjour Rust !"` et que nous voulons extraire uniquement le mot `"Rust"`, nous pouvons utiliser `slice(8, 12)` pour extraire les caractères entre les indices 8 et 12. Voici un exemple de code pour extraire une sous-chaîne en Rust :

```Rust
let chaine = "Bonjour Rust !";
let souschaine = chaine.slice(8, 12);
```

Le résultat de `souschaine` sera `"Rust"`. Nous pouvons également utiliser des variables pour les indices de début et de fin, pour une extraction plus dynamique. Cependant, il est important de noter que `slice()` retournera une référence à la sous-chaîne, et non une copie. Vous devrez donc prendre cela en compte si vous souhaitez modifier la sous-chaîne extraite.

Outre la fonction `slice()`, nous pouvons également utiliser `split()`, qui sépare une chaîne de caractères en plusieurs sous-chaînes à l'aide d'un séparateur spécifique. Par exemple, si nous voulons séparer une chaîne de caractères aux espaces, nous pouvons utiliser la méthode `split(" ")`. Voici un exemple de code :

```Rust
let chaine = "Bonjour Rust !";
let souschaines = chaine.split(" ");
```

Le résultat de `souschaines` sera un tableau avec les sous-chaînes suivantes : `"Bonjour"`,`"Rust"` et `"!"`.

## Deep Dive

Maintenant que nous avons vu les bases de l'extraction de sous-chaînes en Rust, explorons quelques fonctionnalités avancées. Par exemple, nous pouvons spécifier un nombre maximum de sous-chaînes à extraire en utilisant `splitn()`. Nous pouvons également utiliser des expressions régulières pour `split()` et spécifier un délimiteur plus complexe. De plus, en utilisant `split_terminator()`, nous pouvons ignorer les derniers caractères de la chaîne si ceux-ci sont tous des délimiteurs.

Il est également important de noter que les fonctions `split()` et `slice()` fonctionnent également avec d'autres types de données en plus des chaînes de caractères, tels que les vecteurs et les tableaux.

## Voir aussi

Pour en savoir plus sur l'extraction de sous-chaînes en Rust, vous pouvez consulter les ressources suivantes :

- [Documentation sur la méthode `slice()`](https://doc.rust-lang.org/std/primitive.slice.html#method.slice)
- [Documentation sur la fonction `split()`](https://doc.rust-lang.org/std/primitive.str.html#method.split)
- [Documentation sur les expressions régulières en Rust](https://docs.rs/regex/0.2.9/regex/)
- [Guide complet sur la manipulation de chaînes de caractères en Rust](https://stevedonovan.github.io/rust-gentle-intro/6-strings.html)