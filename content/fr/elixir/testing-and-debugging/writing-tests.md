---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:19.768258-07:00
description: "Comment faire : Elixir utilise ExUnit comme son framework de test int\xE9\
  gr\xE9, qui est extr\xEAmement puissant et facile \xE0 utiliser. Voici un exemple\
  \ basique :\u2026"
lastmod: '2024-03-13T22:44:57.330940-06:00'
model: gpt-4-0125-preview
summary: "Elixir utilise ExUnit comme son framework de test int\xE9gr\xE9, qui est\
  \ extr\xEAmement puissant et facile \xE0 utiliser."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Elixir utilise ExUnit comme son framework de test intégré, qui est extrêmement puissant et facile à utiliser. Voici un exemple basique :

1. Créez un nouveau fichier de test dans le répertoire `test` de votre projet Elixir. Par exemple, si vous testez un module nommé `MathOperations`, votre fichier de test pourrait être `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Ceci est un cas de test simple pour vérifier la fonction d'addition
  test "l'addition de deux nombres" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Pour exécuter vos tests, utilisez la commande `mix test` dans votre terminal. Si la fonction `MathOperations.add/2` ajoute correctement deux nombres, vous verrez une sortie similaire à :

```
..

Terminé en 0.03 secondes
1 test, 0 échecs
```

Pour les tests impliquant des services externes ou des API, vous voudrez peut-être utiliser des bibliothèques de simulacres, comme `mox`, pour éviter de toucher les services réels :

1. Ajoutez `mox` à vos dépendances dans `mix.exs` :

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # autres dépendances...
  ]
end
```

2. Définissez un module de simulation dans votre aide de test (`test/test_helper.exs`) :

```elixir
Mox.defmock(HTTPClientMock, pour: HTTPClientBehaviour)
```

3. Utilisez la simulation dans votre cas de test :

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Cela indique à Mox de vérifier que cette simulation a été appelée comme prévu
  setup :verify_on_exit!

  test "obtient des données de l'API" do
    # Préparez la réponse simulée
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Réponse simulée"} end)
    
    assert SomeAPIClient.get_data() == "Réponse simulée"
  end
end
```

Lors de l'exécution de `mix test`, cette configuration vous permet d'isoler vos tests unitaires des véritables dépendances externes, en vous concentrant sur le comportement de votre propre code. Ce schéma garantit que vos tests s'exécutent rapidement et restent fiables, indépendamment du statut des services externes ou de la connectivité Internet.
