---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire des tests, c'est vérifier que notre code fait bien ce qu'on attend de lui. Les programmeurs testent pour prévenir les bugs, économiser du temps et dormir sur leurs deux oreilles.

## How to (Comment faire ?)
Elixir utilise ExUnit pour les tests. Voici un exemple simple :

```elixir
defmodule MathTest do
  use ExUnit.Case
  doctest Math

  test "add/2 function" do
    assert Math.add(1, 2) == 3
  end
end
```

Lancer les tests avec `mix test` et voilà :

```
...

Finished in 0.05 seconds
3 tests, 0 failures

Randomized with seed 54321
```

## Deep Dive (Plongée en profondeur)
Historiquement, Elixir, conçu par José Valim en 2011, a emprunté des bonnes pratiques de Ruby, notamment dans le test avec ExUnit est une proche parente de Ruby's MiniTest. Alternativement, on peut utiliser des frameworks comme ESpec, inspiré de RSpec, pour ceux qui préfèrent une syntaxe plus proche du langage naturel. Les tests sont généralement écrits dans un dossier `test` et suivent la convention `nom_du_module_test.exs`.

## See Also (Voir aussi)
- [Elixir School's Testing Guide](https://elixirschool.com/en/lessons/basics/testing/)
- [ExUnit Documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir's Official Getting Started Guide (Testing)](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
