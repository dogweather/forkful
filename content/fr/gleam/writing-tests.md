---
title:                "Écrire des tests"
html_title:           "Gleam: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi faire des tests?

Les tests de code sont essentiels pour s'assurer que votre code fonctionne correctement et qu'il reste stable au fil du temps. Les programmeurs les utilisent pour vérifier que chaque fonction et chaque partie du code se comportent comme prévu, en s'assurant qu'ils fonctionnent correctement et en détectant les erreurs dès qu'elles se produisent.

# Comment faire des tests en Gleam?

Pour faire des tests en Gleam, utilisez la bibliothèque standard `gleam/test`. Cette bibliothèque fournit des fonctions et des macros pour écrire et exécuter des tests dans votre code.

```Gleam
import gleam/test
import my_project

test "Addition" {
  assert.equal(my_project.add(2, 2), 4)
}
```
La fonction `test` dans `gleam/test` vous permet d'écrire des blocs de code de test auto-descriptifs, avec des noms et des fonctions de test clairs. Vous pouvez ensuite ajouter des assertions pour vérifier que la sortie du code de test est correcte. Dans cet exemple, nous testons la fonction `add` de notre projet, en vérifiant qu'elle ajoute correctement les nombres 2 et 2 et renvoie 4.

# Plongée en profondeur

Écrire des tests pour votre code n'est pas seulement une bonne pratique moderne, mais c'est aussi une méthode éprouvée depuis longtemps pour garantir la qualité du code. Les alternatives telles que le débogage manuel peuvent prendre plus de temps et être moins fiables. De plus, la bibliothèque `gleam/test` est conçue pour être simple et facile à utiliser, ce qui la rend accessible à tous les programmeurs, même débutants.

# Voir aussi

Pour plus d'informations sur l'écriture de tests en Gleam, consultez la documentation officielle sur les tests: https://gleam.run/book/collections/#tests. Vous pouvez également trouver d'autres articles et tutoriels utiles en cherchant ``gleam tests`` sur votre moteur de recherche préféré.