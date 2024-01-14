---
title:                "Gleam: Ecrire des tests"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Gleam

Lorsque l'on travaille sur de grands projets de programmation, il est essentiel de s'assurer que le code fonctionne correctement. Cela peut être un processus long et fastidieux, mais cela en vaut la peine pour éviter les bugs et les erreurs. Heureusement, Gleam offre une solution efficace pour s'assurer que notre code est bien testé. Dans cet article, nous allons explorer pourquoi il est important d'écrire des tests en Gleam et comment le faire.

## Comment écrire des tests en Gleam

Pour commencer, il est important de savoir que les tests en Gleam sont écrits en utilisant le framework d'évaluation interne de Gleam, appelé "ExUnit". Le framework offre un moyen simple et efficace de tester notre code en créant des modules de test séparés.

Pour écrire des tests en Gleam, nous utilisons des blocs de code spéciaux, appelés "```Gleam ... ```", qui nous permettent d'écrire du code Gleam à l'intérieur de blocs Markdown. Ci-dessous, nous pouvons voir un exemple de test qui vérifie si la fonction "add" renvoie le résultat attendu :

```gleam
test "add should return correct result" {
  assert.equal(add(2, 3), 5)
}
```

Dans cet exemple, nous avons utilisé la fonction "assert.equal" pour comparer le résultat de notre fonction "add" avec la valeur attendue de 5. Si le résultat ne correspond pas, le test échouera et nous saurons qu'il y a un problème avec notre fonction "add".

## Plongée en profondeur : Écriture de tests avancés

Maintenant que nous avons vu comment écrire des tests de base en Gleam, il est temps de plonger un peu plus en profondeur. Dans cet article, nous ne couvrirons pas tous les aspects de l'écriture de tests en Gleam, mais nous donnerons un aperçu de certaines fonctionnalités avancées.

Tout d'abord, il est important de noter que les tests peuvent être organisés en sous-groupes en utilisant la syntaxe suivante :

```gleam
suite "addition" {
  test "add should return correct result" {
    assert.equal(add(2, 3), 5)
  }

  test "add should return correct result with negative numbers" {
    assert.equal(add(-2, -3), -5)
  }
}
```

De plus, Gleam nous permet d'écrire des propositions automatiques, également appelées générateurs de données, pour tester notre code avec différentes valeurs d'entrée. Cela peut être très utile pour s'assurer que notre code est robuste et fonctionne correctement avec différents types de données.

## Voir aussi

Maintenant que nous en avons appris un peu plus sur l'écriture de tests en Gleam, voici quelques liens qui pourraient être utiles pour aller plus loin :

- [La documentation officielle de Gleam sur le module ExUnit](https://gleam.run/modules/exunit)
- [Un article sur les tests en Gleam de la communauté Gleam](https://hexdocs.pm/gleam/ExUnit.GettingStarted.html)
- [Le code source de Gleam pour voir comment les tests sont écrits pour le projet lui-même](https://github.com/gleam-lang/gleam)