---
title:    "Elixir: Écrire des tests"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Ecrire des tests en Elixir : pourquoi et comment

## Pourquoi

Ecrire des tests est un aspect crucial du développement logiciel. Il permet de s'assurer que le code est fonctionnel, robuste et peut être modifié en toute sécurité. Avec l'utilisation du langage Elixir, il est important de comprendre à la fois pourquoi et comment écrire des tests pour améliorer la qualité de votre code.

## Comment faire

Pour écrire des tests en Elixir, il existe un framework appelé ExUnit. Ce dernier fournit les outils nécessaires pour créer, exécuter et analyser des tests en Elixir. Voici un exemple d'une fonction simple et de son test correspondant :

```elixir
defmodule Calcul do
  # Une fonction pour calculer le carré
  def sq(num) do
    num * num
  end
end
```

```elixir
defmodule CalculTest do
  use ExUnit.Case

  # On définit un groupe d'exécution pour les tests
  @tag :test
  describe "une fonction de calcul de carré" do
    # On définit le test et on s'assure que le résultat est correct
    test "calcule correctement le carré" do
      assert Calcul.sq(2) == 4
    end
  end
end
```

Après avoir exécuté ces tests, le résultat devrait être le suivant :

```
ExUnit.run(CalculTest)
..

Finished in 0.04 seconds
2 tests, 0 failures
```

Vous pouvez également utiliser des fonctions spéciales comme `assert_raise` pour tester les erreurs, ainsi que les `setup` et `teardown` pour organiser vos tests.

## Plongée en profondeur

Maintenant que vous avez une idée de base sur comment écrire des tests en Elixir, plongeons un peu plus en profondeur. Tout d'abord, il est important de comprendre qu'en Elixir, les tests sont exécutés de façon asynchrone. Cela signifie que chaque test sera exécuté en parallèle et donc que vous devez vous assurer que vos tests soient indépendants les uns des autres pour éviter des problèmes de concurrence.

De plus, il est également important de tester de façon unitaire, c'est-à-dire en se concentrant sur une seule fonction ou un seul module à la fois. Cela rendra vos tests plus clairs et plus faciles à comprendre. Pensez également à tester les différentes situations possibles, y compris les erreurs et les cas limites.

Enfin, n'oubliez pas d'écrire des tests pour chaque nouvelle fonctionnalité que vous ajoutez à votre code. Cela vous aidera à identifier et corriger les erreurs rapidement, ainsi qu'à éviter des regressions.

## Voir aussi

- Guide officiel des tests en Elixir : https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-docs.html
- Documentation de ExUnit : https://hexdocs.pm/ex_unit/ExUnit.html
- Tutoriel vidéo sur les tests en Elixir : https://www.youtube.com/watch?v=N5HvhP8BJ2c&ab_channel=PragmaticReviews