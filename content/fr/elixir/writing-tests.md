---
title:                "Elixir: Ecriture de tests"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en Elixir

Vous vous demandez peut-être pourquoi il est important d'écrire des tests en Elixir. En tant que programmeurs, notre objectif est de créer des applications de haute qualité et sans bugs, qui fonctionnent comme prévu. Les tests jouent un rôle crucial dans ce processus en nous permettant de vérifier que notre code fonctionne correctement et qu'il répond aux exigences.

## Comment faire

Pour écrire des tests en Elixir, nous utilisons le module `ExUnit`, qui est inclus dans le framework standard de Elixir. Commençons par un exemple simple :

```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end
```

Dans cet exemple, nous avons créé un module `Calculator` avec une fonction `add` qui prend deux arguments et retourne leur somme. Maintenant, écrivons un test pour cette fonction en utilisant `ExUnit` :

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "2 plus 2 est égal à 4" do
    assert Calculator.add(2, 2) == 4
  end
end
```

Nous avons utilisé la macro `test` pour définir notre cas de test, qui utilise `assert` pour vérifier que le résultat de `Calculator.add(2, 2)` est égal à 4. Si c'est le cas, le test réussira.

## Plongée en profondeur

Les tests sont écrits pour vérifier le bon fonctionnement de notre code et pour nous donner la confiance nécessaire pour apporter des modifications sans craindre de causer des bugs surprenants. Il existe différents types de tests, tels que les tests unitaires, les tests fonctionnels et les tests d'acceptation. Dans `ExUnit`, nous pouvons utiliser différentes fonctions d'assertion telles que `assert`, `refute` et `assert_raise` pour couvrir différents scénarios de test.

Il est également important de noter que les tests ne sont pas une solution miracle pour éliminer complètement les bugs, mais ils sont un outil précieux pour aider à identifier les problèmes et à maintenir notre code propre et fonctionnel.

## Voir aussi

- [Documentation de ExUnit](https://hexdocs.pm/ex_unit/)
- [Article sur les tests en Elixir](https://dev.to/vadimburlakin/getting-started-with-elixir-what-i-learned-by-writing-unit-tests-t6c)

Merci d'avoir lu cet article sur l'écriture de tests en Elixir. J'espère que cela vous a donné une meilleure compréhension de leur importance et de la manière de les implémenter dans vos projets. N'oubliez pas que les tests sont un outil précieux pour assurer la qualité de votre code et pour vous donner confiance lors de la maintenance et de l'évolution de votre application.