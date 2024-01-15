---
title:                "Écriture de tests"
html_title:           "Rust: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi
Écrire des tests est essentiel pour assurer la qualité de votre code en vous assurant que celui-ci fonctionne correctement et qu'il ne contient pas de bugs. Les tests vous permettent également de trouver des erreurs rapidement et de les corriger avant qu'elles ne deviennent de plus gros problèmes.

## Comment faire
Pour écrire des tests en Rust, vous devez utiliser le framework de test intégré, appelé `test`. Vous pouvez commencer par importer `test` dans votre code comme ceci:

```Rust
use test::test;
```

Ensuite, vous pouvez créer une fonction de test en utilisant l'attribut `#[test]` et en la plaçant avant la déclaration de votre fonction, comme ceci:

```Rust
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}
```

Dans cet exemple, nous avons créé une fonction de test appelée `test_addition` qui teste l'addition de deux nombres en utilisant l'assertion `assert_eq!` pour vérifier qu'ils sont égaux. Vous pouvez exécuter vos tests en exécutant `cargo test` dans votre terminal. Vous verrez une sortie similaire à celle-ci si vos tests réussissent:

```
running 1 test
test test_addition ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Si un test échoue, vous verrez un message d'erreur détaillé vous indiquant ce qui a mal tourné. Par exemple, si nous modifions notre assertion pour qu'elle ne soit pas valide, notre sortie ressemblera à ceci:

```
running 1 test
test test_addition ... FAILED

failures:

---- test_addition stdout ----
________
          | 654321
________|________
         | 109876
-------- | --------
         | 109876
 0 	     | 0
 [..]

failures:
    test_addition

test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

Vous pouvez également ajouter des messages personnalisés à vos assertions pour rendre vos tests plus explicites. Par exemple:

```Rust
#[test]
fn test_multiply() {
    let result = 3 * 2;
    assert_eq!(result, 6, "Expected result to be 6, but found {}", result);
}
```

Dans cet exemple, nous avons ajouté un message qui s'affichera si notre assertion échoue. Si nous changeons la valeur attendue à 9, notre message s'affichera comme ceci:

```
Expected result to be 6, but found 9
```

## Deep Dive
Il existe différents types d'assertions que vous pouvez utiliser dans vos tests en Rust. En plus de `assert_eq!`, qui vérifie si deux valeurs sont égales, vous pouvez également utiliser `assert_ne!` pour vérifier si deux valeurs sont différentes, `assert!(condition)` pour vérifier qu'une condition est vraie, et `assert!(condition, message)` pour vérifier qu'une condition est vraie et afficher un message personnalisé en cas d'échec.

De plus, vous pouvez grouper vos tests en utilisant les macros `#[test]` et `#[test_case]` pour créer des suites de tests et exécuter différents scénarios avec des valeurs différentes.

Les tests en Rust sont également intégrés à la documentation, vous pouvez donc documenter vos tests en utilisant Rustdoc pour générer de la documentation claire pour votre code.

## Voir aussi
- [Documentation Rust sur les tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Guide approfondi sur les tests en Rust](https://www.freecodecamp.org/news/everything-you-need-to-know-about-testing-in-rust-d5d4abf865b4/)
- [Exemples de tests en Rust sur GitHub](https://github.com/rust-lang/rust/tree/master/src/test)