---
title:                "Ruby: Ecrire des tests"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur, vous avez peut-être déjà entendu parler de tests unitaires, tests fonctionnels et autres formes de tests. Mais pourquoi les écrire en premier lieu ? Les tests sont essentiels pour s'assurer de la qualité et de la stabilité de votre code. Ils permettent également de détecter les erreurs plus rapidement et de les corriger avant qu'elles ne causent des problèmes pour les utilisateurs réels.

## Comment faire

Pour écrire des tests en utilisant Ruby, vous pouvez utiliser le framework de test intégré appelé MiniTest. Il suffit de créer un fichier avec l'extension ".rb" et d'utiliser le template suivant :

````Ruby
# importer le framework de test
require "minitest/autorun"

# définir une classe de test
class TestCalculator < Minitest::Test

  # une méthode de test doit commencer par "test_"
  def test_addition
    # tester la méthode d'addition en utilisant l'assertion "assert_equal"
    assert_equal 5, 2 + 3
  end

end
````

Pour lancer les tests, il suffit d'exécuter le fichier avec la commande "ruby" suivie du nom du fichier.

Par exemple, si votre fichier s'appelle "calculator_test.rb", vous pouvez exécuter vos tests en tapant dans votre terminal :

````bash
ruby calculator_test.rb
````

Vous devriez voir une sortie qui indique si les tests ont réussi ou échoué.

## Plongée en profondeur

Il existe différents types de tests, chacun ayant un objectif spécifique. Les tests unitaires permettent de vérifier que chaque morceau de code fonctionne correctement de manière isolée. Les tests d'intégration sont utilisés pour vérifier que toutes les parties du code communiquent bien entre elles. Les tests fonctionnels sont conçus pour tester le code du point de vue de l'utilisateur. Avec tous ces différents types de tests, il est possible de s'assurer de la robustesse et de la cohérence de votre code.

De plus, en utilisant des tests automatisés, vous pourrez économiser du temps et de l'énergie en évitant de devoir tester manuellement votre code à chaque modification.

## Voir aussi

- [Documentation officielle Ruby](https://www.ruby-lang.org/fr/documentation/)
- [Tutoriel sur les tests en Ruby](https://bundler.io/guides/testing.html)