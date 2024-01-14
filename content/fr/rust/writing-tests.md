---
title:    "Rust: Écriture des tests"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Rust ?

Les tests sont un élément crucial dans le développement de logiciels en Rust. Ils permettent de détecter rapidement des erreurs et de garantir la fiabilité et la stabilité du code. En écrivant des tests, vous pouvez avoir confiance en votre code et le rendre plus facile à maintenir à long terme.

## Comment écrire des tests en Rust ?

Pour écrire des tests en Rust, vous devez tout d'abord définir une fonction avec l'attribut `#[test]` au-dessus de celle-ci. À l'intérieur de cette fonction, vous pouvez écrire des assertions qui vérifient que votre code se comporte comme prévu. Par exemple :

```Rust
#[test]
fn test_addition() {
    let result = 2 + 2;
    assert_eq!(result, 4);
}
```

Une fois que vous avez écrit votre test, vous pouvez l'exécuter en utilisant la commande `cargo test`, qui exécute tous les tests contenus dans votre projet. Vous verrez alors l'output suivant :

```
running 1 test
test test_addition ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Si le test échoue, vous verrez un message d'erreur détaillé, ce qui vous permet de facilement identifier et corriger le problème.

## Plongée en profondeur dans l'écriture des tests

Les tests en Rust peuvent être plus complexes que des simples assertions. Vous pouvez utiliser un macro appelé `assert!` pour vérifier des conditions booléennes, ou encore utiliser le module `assert_eq!` pour comparer deux valeurs. De plus, Rust inclut également des fonctionnalités telles que `#[should_panic]` qui vous permettent de tester le comportement en cas d'erreur.

De plus, vous pouvez également utiliser des tests pour documenter votre code et montrer son fonctionnement de manière précise et reproductible.

# Voir aussi

Voici quelques ressources supplémentaires pour en savoir plus sur l'écriture de tests en Rust :

- La documentation officielle de Rust sur les tests : https://doc.rust-lang.org/book/ch11-01-writing-tests.html
- Un tutoriel complet sur l'écriture de tests en Rust : https://www.clifford.at/rust_test/