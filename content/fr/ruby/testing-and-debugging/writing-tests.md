---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:46.293863-07:00
description: "Comment faire : Ruby est fourni avec une biblioth\xE8que int\xE9gr\xE9\
  e appel\xE9e `Test::Unit` pour \xE9crire des tests unitaires, encapsulant les pratiques\
  \ de test\u2026"
lastmod: '2024-03-13T22:44:58.423526-06:00'
model: gpt-4-0125-preview
summary: "Ruby est fourni avec une biblioth\xE8que int\xE9gr\xE9e appel\xE9e `Test::Unit`\
  \ pour \xE9crire des tests unitaires, encapsulant les pratiques de test dans des\
  \ structures simples."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Ruby est fourni avec une bibliothèque intégrée appelée `Test::Unit` pour écrire des tests unitaires, encapsulant les pratiques de test dans des structures simples. Cependant, la communauté Ruby a souvent une préférence pour des bibliothèques tierces comme RSpec et Minitest en raison de leur expressivité et flexibilité accrues.

### Utiliser `Test::Unit` :
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Exécutez votre fichier de test depuis le terminal, et vous devriez obtenir une sortie indiquant le succès ou l'échec des tests :
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Utiliser RSpec :
RSpec est un cadre populaire de BDD (Développement Dirigé par le Comportement) pour Ruby. Installez le gem avec `gem install rspec`, puis initialisez-le dans votre projet avec `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'ajoute correctement deux nombres' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Exécutez les tests avec la commande `rspec`. Exemple de sortie :
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Utiliser Minitest :
Minitest fournit une suite complète de facilités de test supportant le TDD, BDD, le mocking et le benchmarking. Installez-le avec `gem install minitest` et utilisez-le comme suit :

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

Exécutez votre fichier de test directement ou via la tâche `rake` configurée pour minitest. Exemple de sortie :
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

En mettant en œuvre des tests dans vos projets Ruby en utilisant ces bibliothèques, vous adhérez aux meilleures pratiques, menant à des bases de code plus fiables et maintenables.
