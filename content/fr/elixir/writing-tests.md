---
title:                "Elixir: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en Elixir?

L'écriture de tests est une pratique courante dans le développement de logiciels pour vérifier le bon fonctionnement du code et prévenir les erreurs. En Elixir, l'écriture de tests peut également aider à garantir que toutes les fonctionnalités du langage sont utilisées correctement et contribuer à maintenir un code propre et facile à débuguer.

## Comment écrire des tests en Elixir

Pour écrire des tests en Elixir, il est nécessaire d'utiliser le module `ExUnit`, qui est inclus dans la bibliothèque standard du langage. Voici un exemple de fonction Elixir à tester :

```Elixir
def add(a, b) do
  a + b
end
```

Pour tester cette fonction, on peut utiliser `assert` dans un module de test `ExampleTest`.

```Elixir
defmodule ExampleTest do
  use ExUnit.Case
  
  test "addition" do
    result = add(3, 5)
    assert result == 8
  end
end
```

Lorsque nous exécutons le test avec `mix test`, nous obtenons le message suivant :

```
==> Example
Finished in 0.03 seconds (0.02s on load, 0.01s on tests)
1 tests, 0 failures
```

Cela signifie que notre test s'est exécuté avec succès sans aucun échec. Maintenant, si nous voulons tester un résultat incorrect, nous pouvons modifier notre assertion ainsi :

```Elixir
assert result == 7
```

Et si nous exécutons le test, nous obtenons cette fois-ci un message indiquant un échec :

```
==> Example
Finished in 0.03 seconds (0.02s on load, 0.01s on tests)
1 tests, 1 failures
```

De cette façon, en écrivant des tests pour chaque fonction de notre code, nous pouvons nous assurer que notre code fonctionne correctement et détecter rapidement les erreurs.

## Approfondir l'écriture de tests

En plus des tests unitaires comme celui que nous avons vu précédemment, il existe d'autres types de tests en Elixir, tels que les tests d'intégration et les tests de bout en bout. Ces différents types de tests peuvent être utilisés en combinaison pour garantir la qualité du code et la couverture des fonctionnalités. De plus, il est également possible de créer des tests plus complexes en utilisant différentes méthodes de test telles que `setup_all` et `teardown_all` pour préparer et nettoyer l'environnement de test.

## Voir aussi

Pour en savoir plus sur l'écriture de tests en Elixir, vous pouvez consulter les ressources suivantes :

- [Documentation officielle Elixir - Tests](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-ct.html)
- [Formation Udemy - Tests en Elixir](https://www.udemy.com/course/elixir-for-beginners/learn/lecture/4945132?start=0#overview)
- [Blog Programez.com - Écrire des tests en Elixir](https://www.programmez.com/etudes/un-petit-tour-dhorizon-des-tests-en-elixir-26865)

Maintenant que vous connaissez les bases de l'écriture de tests en Elixir, n'hésitez pas à les utiliser dans votre prochain projet pour garantir un code de qualité et facile à maintenir. Happy coding!