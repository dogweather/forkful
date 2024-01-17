---
title:                "Écrire des tests"
html_title:           "Elixir: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-tests.md"
---

{{< edit_this_page >}}

Qu'est-ce que les tests et pourquoi les programmeurs les font-ils?

Les tests en programmation sont des morceaux de code destinés à vérifier que notre code fonctionne correctement et qu'il produit les résultats escomptés. Ils peuvent également aider à détecter et résoudre des bugs ou des erreurs de manière proactive. Les programmeurs les écrivent également pour s'assurer que les modifications apportées à un code existant ne causent pas de régressions.

Comment procéder:

Elixir est un langage qui encourage fortement la programmation basée sur les tests, ce qui signifie qu'il met à disposition des outils pour faciliter la création et l'exécution de tests automatisés.

Voici un exemple de test en Elixir:

```Elixir
defmodule CalculatriceTest do
  use ExUnit.Case

  test "additionne deux nombres" do
    assert Calculatrice.ajouter(2, 3) == 5
  end
end
```

L'exemple ci-dessus montre un cas de test basique, utilisant le module ExUnit qui est inclus dans la standard library d'Elixir. Nous procédons en définissant un module pour nos tests, en utilisant le `use` pour inclure le module `ExUnit.Case` et enfin, nous écrivons un test spécifique à l'aide du mot-clé `test`.

Nous pouvons exécuter ces tests en exécutant la commande `mix test` dans notre terminal, ce qui nous donnera un retour sur le succès ou l'échec de notre test.

Plongée en profondeur:

La pratique des tests en programmation remonte aux premiers jours de l'informatique. Avec l'avènement des méthodologies agiles et de l'importance croissante de la qualité logicielle, les tests sont devenus une partie essentielle du processus de développement. Il existe également d'autres outils pour tester du code en Elixir, tels que le framework de test intégré à Phoenix, le framework de web development en Elixir.

Voir aussi:

- La documentation officielle d'ExUnit: https://hexdocs.pm/ex_unit/
- Article sur les tests automatisés en Elixir: https://dockyard.com/blog/2016/01/28/writing-tests-and-defining-test-data-in-elixir