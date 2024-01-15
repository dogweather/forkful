---
title:                "Rédiger des tests"
html_title:           "Gleam: Rédiger des tests"
simple_title:         "Rédiger des tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire des tests peut sembler fastidieux et ennuyeux, mais c'est en fait un élément crucial dans le processus de développement d'un logiciel de qualité. Les tests permettent de s'assurer que votre code fonctionne correctement, évitant ainsi les éventuels bugs et erreurs, et facilitent la maintenance du code à long terme.

## Comment faire

Pour écrire des tests en Gleam, il suffit d'utiliser le module `test` et ses différentes fonctions. Par exemple, pour tester une fonction qui calcule le carré d'un nombre, on peut écrire le code suivant:

```
Gleam
import test

fn square(x) {
  x * x
}

test.suite("Tests de la fonction square", [
  test.test("Retourne le carré d'un nombre positif", _ {
    expect(square(5)).toBe(25)
  }),
  test.test("Retourne le carré d'un nombre négatif", _ {
    expect(square(-3)).toBe(9)
  })
])
```

Une fois exécuté, ce code va effectuer les deux tests définis et afficher le résultat (`Pass` ou `Fail`) pour chaque test.

## Plongeons plus en profondeur

Lors de l'écriture de tests, il est important de couvrir différentes situations et cas de figure. Pour cela, on peut utiliser des fonctions d'assertion telles que `expect` et `assert` pour vérifier que les résultats obtenus correspondent à ceux attendus. Le module `test` offre également la possibilité de créer des suites de tests pour organiser votre code et faciliter la lisibilité.

Il est également recommandé d'écrire des tests automatisés, qui peuvent être exécutés à chaque modification du code pour s'assurer que tout fonctionne correctement. Cela permet de détecter rapidement les erreurs et de maintenir la qualité de votre code au fil du temps.

## Voir aussi

- [Documentation officielle de Gleam sur les tests](https://gleam.run/book/testing.html)
- [Article sur l'importance des tests en développement logiciel](https://www.codespot.org/importance-des-tests-en-developpement-logiciel/)
- [Tutoriel sur les tests automatisés en Gleam](https://www.youtube.com/watch?v=ssv6fybWuTY)