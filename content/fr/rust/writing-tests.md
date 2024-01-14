---
title:    "Rust: Ecrire des tests"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Les tests sont un élément crucial dans le processus de développement de logiciels. Ils permettent de vérifier la fonctionnalité de notre code et de s'assurer qu'il fonctionne comme prévu. En utilisant Rust, un langage de programmation moderne et performant, écrire des tests peut grandement améliorer la qualité de notre code et rendre le processus de développement plus efficace. Dans cet article, nous allons découvrir pourquoi écrire des tests en Rust est si important et comment le faire correctement.

## Comment faire

Nous allons commencer par un exemple simple pour illustrer comment écrire des tests en Rust. Supposons que nous ayons une fonction qui prend en entrée un nombre entier et renvoie `true` si ce nombre est pair, sinon renvoie `false`.

```Rust
fn est_pair(nombre: i32) -> bool {
    nombre % 2 == 0
}

#[test]
fn test_est_pair() {
    assert!(est_pair(4));
    assert!(!est_pair(5));
}
```

Dans cet exemple, nous avons créé une fonction `est_pair` et un test pour vérifier son fonctionnement. Nous utilisons la macro `#[test]` pour indiquer que cette fonction est un test. À l'intérieur du test, nous utilisons la macro `assert!` qui vérifie si l'expression à l'intérieur est évaluée à `true`. Si c'est le cas, le test réussit. Sinon, il échoue. Dans cet exemple, nous avons ajouté deux assertions pour tester la fonction `est_pair` avec différents nombres. Vous pouvez tester ce code en exécutant la commande `cargo test` dans votre terminal.

## Plongée en profondeur

Il existe plusieurs types de tests que vous pouvez écrire en utilisant Rust, tels que les tests unitaires, les tests d'intégration et les tests de propriété. Les tests unitaires sont destinés à tester une fonction ou une partie spécifique de votre code. Les tests d'intégration sont utilisés pour s'assurer que différentes parties de votre code fonctionnent bien ensemble. Les tests de propriété, quant à eux, sont utilisés pour vérifier des propriétés spécifiques de votre code, telles que la conformité à des invariants ou des comportements attendus.

Pour écrire des tests de manière efficace en Rust, il est important de comprendre les macros `assert!` et `assert_eq!` qui vous permettent de vérifier si une expression est vraie ou si deux valeurs sont égales respectivement. Vous pouvez également utiliser des fonctions utilitaires telles que `assert_ne!` pour vérifier si deux valeurs sont différentes, ou `panic!` pour arrêter l'exécution d'un test et afficher un message d'erreur.

Enfin, n'oubliez pas que les tests doivent être maintenus et mis à jour au fur et à mesure que votre code évolue. Ne pas maintenir des tests peut entraîner des erreurs et rendre votre code instable.

## Voir aussi

- [Guide officiel de Rust sur les tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Vidéo YouTube sur les tests en Rust](https://www.youtube.com/watch?v=JFwPVK_2-Jw)
- [Article de blog sur les tests de propriété en Rust](https://medium.com/@carlosfaria3/property-testing-in-rust-using-proptest-part-1-ef534607bdb9)