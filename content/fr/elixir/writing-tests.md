---
title:    "Elixir: Écriture des tests"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi 

L'écriture de tests est une pratique courante dans le développement de logiciels, et cela pour une bonne raison. En écrivant des tests pour votre code Elixir, vous vous assurez qu'il fonctionne de la manière prévue et qu'il n'y a pas de bugs cachés. Cela permet également de faciliter la maintenance et les mises à jour de votre code.

## Comment faire

Pour écrire des tests en Elixir, nous allons utiliser le module ExUnit. Nous allons prendre l'exemple d'une fonction pour calculer la moyenne d'une liste de nombres :

```Elixir
defmodule Calculator do
  def average(numbers) do
    sum = Enum.sum(numbers)
    n = length(numbers)
    sum / n
  end
end
```

Nous voulons maintenant écrire des tests pour cette fonction. Voici comment cela pourrait ressembler avec ExUnit :

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case
  doctest Calculator

  test "average/1 returns the correct average" do
    assert Calculator.average([1, 2, 3]) == 2
  end

  test "average/1 with an empty list returns 0" do
    assert Calculator.average([]) == 0
  end

  test "average/1 with negative numbers returns a negative average" do
    assert Calculator.average([-1, -2, -3]) == -2
  end
end
```

En écrivant des tests pour différentes valeurs d'entrée, nous nous assurons que notre fonction fonctionne correctement dans tous les cas. Maintenant, si nous apportons des modifications à notre fonction, nous pouvons facilement exécuter nos tests et vérifier si tout fonctionne toujours comme prévu.

Pour exécuter ces tests, il suffit de lancer la commande `mix test` dans le répertoire de votre projet.

## Plongée en profondeur

Maintenant que nous savons comment écrire des tests en Elixir, nous pouvons également explorer les différentes options et fonctionnalités qu'offre ExUnit. Par exemple, nous pouvons utiliser les méthodes `setup` et `teardown` pour mettre en place et nettoyer des ressources avant et après l'exécution de nos tests. Nous pouvons également utiliser les tags pour exécuter des tests spécifiques ou pour définir des conditions de réussite pour certains tests.

Il est également important de noter que les tests en Elixir peuvent être facilement parallélisés, ce qui permet d'accélérer considérablement l'exécution de vos tests.

Enfin, il est recommandé de garder vos tests courts, précis et faciles à lire. Vos tests doivent être des exemples clairs et concis de comment votre code devrait fonctionner. Si vos tests sont trop compliqués, cela peut indiquer un problème dans votre code et ils deviendront également plus difficiles à maintenir.

## Voir aussi

- [La documentation officielle d'ExUnit](https://hexdocs.pm/ex_unit/master/ExUnit.html)
- [Un guide détaillé sur les tests en Elixir](https://elixir-lang.org/getting-started/introduction-to-mix.html#tests)
- [Un article sur les tests parallèles en Elixir](https://medium.com/elixir-radar/parallel-tests-with-exunit-in-elixir-1-9-aa88810b6655)