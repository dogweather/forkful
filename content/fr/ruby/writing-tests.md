---
title:                "Écriture de tests"
html_title:           "Ruby: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Ruby et que vous passez des heures à écrire du code, vous savez à quel point il peut être frustrant de découvrir des bugs lors de l'exécution de votre programme. C'est là qu'interviennent les tests - ils vous aident à identifier et à corriger ces bugs dès le départ, vous faisant gagner du temps et des maux de tête à long terme.

## Comment faire

Pour écrire des tests en Ruby, vous pouvez utiliser le framework de test intégré appelé MiniTest. Voici un exemple de test pour une méthode qui calcule la somme de deux nombres :

```Ruby
require 'minitest/autorun'

def sum(a, b)
  a + b
end

class TestSum < Minitest::Test
  def test_sum
    assert_equal 5, sum(2, 3)
  end
end
```

Lorsque vous exécutez le test, vous verrez le résultat suivant :

```
Run options: --seed 32810

# Running:

.

Finished in 0.000887s, 1126.5245 runs/s, 1126.5245 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Comme vous pouvez le voir, le test a été réussi avec une assertion valide. Vous pouvez ajouter d'autres tests pour couvrir plusieurs cas d'utilisation et vous assurer que votre méthode fonctionne correctement.

## Plongée en profondeur

Écrire des tests efficaces nécessite une bonne connaissance du comportement attendu de votre code. Il est important de tester différents cas d'utilisation, y compris les entrées invalides, pour vous assurer que votre code peut gérer toutes les situations possibles.

Vous pouvez également profiter de la méthodologie TDD (Test-Driven Development) en écrivant d'abord les tests pour définir le comportement souhaité, puis en écrivant le code pour passer ces tests. Cela peut vous aider à créer un code plus propre et à éviter les bugs à l'avenir.

## Voir aussi

- [Documentation MiniTest](https://ruby-doc.org/stdlib-2.7.2/libdoc/minitest/rdoc/MiniTest.html)
- [Guide TDD en Ruby](https://thoughtbot.com/upcase/test-driven-rails-resources/tdd)
- [Tutoriel sur les tests en Ruby](https://www.rubyguides.com/2016/02/rspec-ruby-testing-tutorial/)