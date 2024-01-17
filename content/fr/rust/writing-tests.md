---
title:                "Ecrire des tests"
html_title:           "Rust: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi faire ?

Écrire des tests en programmation est une méthode permettant de vérifier que le code fonctionne correctement et de s'assurer qu'aucun bogue n'est introduit lors de futures modifications. Les programmeurs le font pour garantir la qualité et la fiabilité de leur code et pour détecter rapidement les erreurs.

## Comment faire :

Le code suivant montre un exemple de fonction de test en Rust :

```Rust
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}
```

Le résultat attendu de ce test est que 2 plus 2 soit égal à 4. Pour exécuter ce test, vous pouvez utiliser la commande `cargo test` dans votre terminal. Si le test est réussi, vous verrez un message indiquant "test test_addition ... ok". Sinon, vous recevrez un message d'erreur indiquant quel résultat était attendu et le résultat obtenu.

## Plongée en profondeur :

L'écriture de tests est une pratique courante dans le développement logiciel moderne, mais cela n'a pas toujours été le cas. À l'origine, les tests étaient souvent effectués manuellement, ce qui était fastidieux et sujet aux erreurs. C'est pourquoi des frameworks de tests automatiques ont été développés pour faciliter cette tâche.

Bien qu'il existe plusieurs frameworks de tests en Rust, le plus couramment utilisé est `cargo test`. Celui-ci crée un exécutable séparé pour vos tests et les exécute en parallèle, ce qui permet d'économiser du temps lors de l'exécution d'un grand nombre de tests.

## À voir également :

- La documentation officielle de Rust sur les tests : https://doc.rust-lang.org/book/ch11-01-writing-tests.html
- Un article de blog sur l'importance des tests en programmation : https://blog.newrelic.com/2016/09/22/unit-testing-tdd-benefits/
- La bibliothèque de tests unitaires de Rust, `assert`, pour en savoir plus sur les différents types de tests : https://doc.rust-lang.org/std/macro.assert.html