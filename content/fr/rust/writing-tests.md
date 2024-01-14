---
title:                "Rust: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi
Les tests sont un aspect essentiel de toute programmation. Ils permettent de vérifier que le code fonctionne correctement et de s'assurer qu'aucun bug n'est introduit lors de modifications ultérieures. En écrivant des tests pour votre code Rust, vous pouvez avoir plus de confiance dans sa qualité et sa stabilité.

## Comment faire
Pour écrire des tests en Rust, il faut utiliser le module `test`. Voici un exemple de fonction de test simple :

```Rust
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}
```

Ce code crée une fonction de test avec l'attribut `#[test]`, qui signifie que cette fonction sera exécutée lorsque vous lancez vos tests. La fonction utilise ensuite l'assertion `assert_eq!`, qui vérifie que l'expression `2 + 2` est égale à `4`. Si c'est le cas, le test passe, sinon il échoue.

Vous pouvez également utiliser les macros `assert!` et `assert_ne!` pour vérifier si une expression est vraie ou fausse. De plus, vous pouvez également utiliser le module `should_panic` pour tester si une fonction lève une erreur lorsqu'elle est censée le faire.

## Deep Dive
Les tests en Rust peuvent être divisés en trois types : les tests unitaires, les tests d'intégration et les tests de performance.

Les tests unitaires sont des petits tests qui vérifient une seule fonction ou un seul module spécifique. Ils sont écrits par les développeurs et sont exécutés automatiquement chaque fois que du code est modifié ou lors de la compilation.

Les tests d'intégration, quant à eux, vérifient comment différentes parties de code fonctionnent ensemble. Ils sont souvent utilisés pour tester les interfaces entre différents modules ou pour vérifier le fonctionnement du code avec des dépendances externes.

Enfin, les tests de performance vérifient si le code répond aux exigences de performances, tels que le temps d'exécution ou l'utilisation de la mémoire.

## Voir aussi
- [The Rust Book - Writing Automated Tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [Rustlings - Writing Tests](https://github.com/rust-lang/rustlings/blob/main/exercises/test1/README.md)
- [Rust Reddit - Writing Tests](https://www.reddit.com/r/rust/comments/fxevy5/how_do_you_write_tests_in_rust/)