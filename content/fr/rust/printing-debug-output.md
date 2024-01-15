---
title:                "Impression de sortie de débogage"
html_title:           "Rust: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginons que vous écrivez un programme en Rust et que vous rencontrez des problèmes lors de l'exécution. Il est souvent difficile de comprendre où exactement les erreurs se produisent et pourquoi. C'est là que l'impression du débogage entre en jeu ! En imprimant des messages de débogage dans votre code, vous pouvez mieux comprendre ce qui se passe et ainsi trouver plus facilement les erreurs.

## Comment faire

Tout d'abord, pour imprimer du débogage dans votre code Rust, vous pouvez utiliser la macro `println!()` avec un message personnalisé entre les guillemets. Par exemple :

```Rust
let chiffre = 5;
println!("Le chiffre est {}", chiffre);
```

Cela affichera "Le chiffre est 5" dans la console lorsque vous exécutez votre programme. Vous pouvez également utiliser la macro `dbg!()` pour imprimer une valeur et son nom de variable, comme ceci :

```Rust
let nom = "John";
dbg!(nom);
```

Ce qui affichera "nom: "John"" dans la console. Vous pouvez également utiliser `dbg!()` à l'intérieur d'une fonction pour imprimer plusieurs valeurs à la fois.

## Plongée en profondeur

En plus de simplement imprimer des messages de débogage, vous pouvez également utiliser `dbg!()` pour mettre en pause l'exécution de votre code et inspecter les valeurs des variables en temps réel. Pour ce faire, il suffit d'ajouter cette macro à l'endroit souhaité dans votre code et de déboguer en mode "run" ou "debug" dans votre environnement de développement.

Il existe également d'autres macros utiles pour l'impression de débogage en Rust, telles que `debug_assert!()` pour vérifier des conditions pendant le débogage et `eprintln!()` pour imprimer des messages de débogage dans la sortie d'erreur.

## Voir aussi

- [Documentation officielle sur l'impression de débogage en Rust](https://doc.rust-lang.org/std/macro.dbg.html)
- [Article sur la détection des erreurs avec les macros de débogage en Rust](https://blog.logrocket.com/error-handling-macros-rust/)

Maintenant que vous savez comment imprimer du débogage en Rust, vous pouvez mieux comprendre et résoudre les erreurs dans votre code ! Continuez à explorer les fonctionnalités de débogage en Rust pour améliorer encore plus votre processus de développement.