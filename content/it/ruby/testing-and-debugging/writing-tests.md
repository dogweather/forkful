---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:55.105294-07:00
description: "Nel contesto di Ruby, il testing si riferisce alla verifica che il codice\
  \ si comporti come previsto in varie condizioni. I programmatori scrivono test per\u2026"
lastmod: '2024-03-13T22:44:44.054802-06:00'
model: gpt-4-0125-preview
summary: Nel contesto di Ruby, il testing si riferisce alla verifica che il codice
  si comporti come previsto in varie condizioni.
title: Scrivere test
weight: 36
---

## Cos'è & Perché?
Nel contesto di Ruby, il testing si riferisce alla verifica che il codice si comporti come previsto in varie condizioni. I programmatori scrivono test per assicurare la correttezza, prevenire regressioni e facilitare il refactoring, con l'obiettivo di realizzare applicazioni robuste e mantenibili.

## Come fare:
Ruby include una libreria integrata chiamata `Test::Unit` per scrivere test unitari, incapsulando le pratiche di testing in strutture semplici. Tuttavia, la comunità Ruby tende spesso a preferire librerie di terze parti come RSpec e Minitest per la loro maggiore espressività e flessibilità.

### Usare `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Esegui il tuo file di test dal terminale, e dovresti ottenere un output che indica il successo o il fallimento dei test:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Usare RSpec:
RSpec è un popolare framework BDD (Behavior-Driven Development) per Ruby. Installa la gemma con `gem install rspec`, poi inizializzala nel tuo progetto con `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'aggiunge correttamente due numeri' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Esegui i test con il comando `rspec`. Esempio di output:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Usare Minitest:
Minitest fornisce una suite completa di strumenti di testing che supporta TDD, BDD, mocking e benchmarking. Installala con `gem install minitest` e usala come segue:

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

Esegui direttamente il tuo file di test o tramite il task `rake` impostato per minitest. Output di esempio:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Implementando i test nei tuoi progetti Ruby utilizzando queste librerie, aderisci alle migliori pratiche, portando a basi di codice più affidabili e mantenibili.
