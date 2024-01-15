---
title:                "Écriture de tests"
html_title:           "Elixir: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests est essentiel pour garantir la fiabilité et la stabilité de nos programmes. De plus, cela permet de détecter rapidement les erreurs et de les corriger avant qu'elles ne deviennent de plus gros problèmes.

## Comment faire

Ecrire des tests en Elixir est simple et intuitif grâce à son framework de tests intégré appelé ExUnit. Voici un exemple de test pour une fonction qui retourne la somme de deux nombres :

```Elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "add" do
    assert Calculator.add(2, 5) == 7
  end
end
```

Vous pouvez exécuter ce test en utilisant la commande `mix test`, qui exécutera tous les tests présents dans votre projet. Le résultat devrait être le suivant :

```
...

Finished in 0.04 seconds
1 test, 0 failures

Randomized with seed 605633
```

Nous pouvons voir que notre test a réussi. Cependant, imaginons que nous passions les mauvais arguments à notre fonction `add`. Notre test échouera alors et nous pourrons immédiatement détecter et corriger l'erreur.

## Plongeon en profondeur

En plus de tester les fonctions individuelles, il est également important de tester l'interaction entre différentes parties de votre code. Cela peut être fait en utilisant des mocks et des stubs pour simuler les comportements de certaines fonctions ou modules. Vous pouvez également utiliser des outils comme `mix cover` pour mesurer la couverture de vos tests et vous assurer qu'ils couvrent suffisamment de code.

Vous devriez également avoir un bon équilibre entre les tests unitaires, qui testent une seule fonction ou un seul module, et les tests d'intégration, qui testent l'ensemble de votre système. Les tests d'intégration peuvent être effectués à l'aide d'outils comme `Hound` pour tester l'interface utilisateur de votre application.

## Voir aussi

- [Documentation officielle d'ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Tutoriel sur les tests en Elixir](https://elixir-lang.org/getting-started/testing.html)
- [Elixir School - ExUnit](https://elixirschool.com/lessons/specifics/exunit/)