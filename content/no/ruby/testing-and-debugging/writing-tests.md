---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:58.148648-07:00
description: "Testing i Ruby handler om \xE5 verifisere at koden din oppf\xF8rer seg\
  \ som forventet under ulike forhold. Programmerere skriver tester for \xE5 sikre\
  \ korrekthet,\u2026"
lastmod: '2024-03-13T22:44:41.331866-06:00'
model: gpt-4-0125-preview
summary: "Testing i Ruby handler om \xE5 verifisere at koden din oppf\xF8rer seg som\
  \ forventet under ulike forhold."
title: Skrive tester
weight: 36
---

## Hvordan:
Ruby kommer med et innebygget bibliotek kalt `Test::Unit` for å skrive enhetstester, som inkapsulerer testingpraksiser innenfor greie strukturer. Imidlertid heller Ruby-samfunnet ofte mot tredjepartsbiblioteker som RSpec og Minitest på grunn av deres forbedrede ekspressivitet og fleksibilitet.

### Bruke `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    resultat = 2 + 2
    assert_equal 4, resultat
  end
end
```
Kjør testfilen din fra terminalen, og du bør få en utskrift som indikerer suksess eller feil ved testene:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Bruke RSpec:
RSpec er et populært BDD (Behavior-Driven Development, atferdsdreven utvikling) rammeverk for Ruby. Installer gem-en med `gem install rspec`, deretter initialiser den i prosjektet ditt med `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'legger korrekt sammen to tall' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Kjør tester med `rspec` kommandoen. Eksempel på utskrift:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Bruke Minitest:
Minitest tilbyr en komplett pakke av testfasiliteter som støtter TDD (Test-Driven Development, testdreven utvikling), BDD, mocking, og benchmarking. Installer det med `gem install minitest` og bruk som følger:

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

Kjør testfilen din direkte eller gjennom `rake` oppgaven satt opp for Minitest. Eksempel på utskrift:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Ved å implementere tester i dine Ruby-prosjekter ved hjelp av disse bibliotekene, følger du beste praksiser, som fører til mer pålitelige og vedlikeholdbare kodebaser.
