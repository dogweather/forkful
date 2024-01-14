---
title:    "Rust: Écrire sur le flux d'erreur standard"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation est d'afficher des informations à l'utilisateur. Que ce soit pour déboguer du code, signaler des erreurs ou simplement donner des instructions, il est essentiel d'avoir un bon moyen de communiquer avec l'utilisateur. Dans cette optique, écrire dans la sortie standard (stdout) peut sembler être une solution simple et rapide. Cependant, il existe une autre option souvent négligée: écrire dans la sortie d'erreur (stderr). Dans cet article, nous allons explorer pourquoi écrire dans stderr peut être utile et comment le faire en utilisant Rust.

## Comment faire

Pour écrire dans la sortie d'erreur en Rust, nous pouvons utiliser la macro `eprintln!`. Cette macro fonctionne de la même manière que `println!` (pour écrire dans stdout), mais au lieu d'afficher le résultat dans la console, elle l'écrit dans stderr. Voyons cela en action avec un exemple:

```
fn main() {
    let name = "Jean";
    eprintln!("Bonjour, {}!", name);
}
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant dans notre terminal:

```
Bonjour, Jean!
```

Comme vous pouvez le voir, la différence entre `println!` et `eprintln!` est simplement la lettre "e" ajoutée au début. Cela peut sembler mineur, mais cela peut faire une grande différence dans certaines situations. Par exemple, si notre programme a été appelé à partir d'un autre programme qui redirige la sortie d'erreur vers un fichier de log, les informations que nous avons écrites avec `eprintln!` seront visibles dans ce fichier, tandis que les informations écrites avec `println!` ne le seront pas.

## Plongée en profondeur

Il y a plusieurs raisons pour lesquelles écrire dans la sortie d'erreur peut être avantageux. La première est liée aux performances. Écrire dans stderr est généralement plus rapide que d'écrire dans stdout, car stdin/stdout/stderr sont des file descriptors sur les systèmes d'exploitation tels que Unix. Cela signifie qu'il y a moins de traitements impliqués et donc moins de latence. De plus, si notre programme effectue des opérations simultanées sur stdout et stderr, les messages peuvent être mélangés. Cela peut rendre le débogage plus difficile et causer des problèmes lors de l'utilisation d'outils de capture de la console.

Une autre raison est que la sortie d'erreur est souvent utilisée pour signaler des erreurs. Si nous voulons que ces erreurs soient plus visibles pour l'utilisateur final, nous pouvons les écrire dans stderr plutôt que dans stdout. Cela rendra les erreurs plus apparentes et plus faciles à repérer pour les utilisateurs.

## Voir aussi

- [Documentation officielle sur eprintln!](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Différences entre stdout et stderr](https://www.geeksforgeeks.org/difference-between-stdout-and-stderr/)
- [Guide de référence Rust sur les entrées/sorties](https://doc.rust-lang.org/std/io/index.html)