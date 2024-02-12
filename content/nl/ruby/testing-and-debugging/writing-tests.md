---
title:                "Tests Schrijven"
aliases:
- /nl/ruby/writing-tests/
date:                  2024-01-28T22:13:36.206298-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven controleert of code werkt zoals verwacht. Programmeurs doen dit om fouten vroegtijdig op te sporen, betrouwbaarheid te garanderen en toekomstige code wijzigingen te vergemakkelijken.

## Hoe:

Ruby gebruikt Minitest en RSpec voor het testen - laten we RSpec gebruiken. Installeer het eerst:

```ruby
gem install rspec
```

Maak een testbestand, `calculator_spec.rb`:

```ruby
RSpec.describe Calculator do
  describe "#add" do
    it "somt twee getallen op" do
      expect(Calculator.new.add(3, 7)).to eql(10)
    end
  end
end
```

Voer de test uit met:

```shell
rspec calculator_spec.rb
```

Uitvoer:

```
F

Mislukkingen:

  1) Calculator#add somt twee getallen op
     Mislukking/Fout: verwachtte dat (Calculator.new.add(3, 7)).to eql(10)
     
     NameError:
       niet-geïnitialiseerde constante Calculator
```

Maak `calculator.rb`:

```ruby
class Calculator
  def add(a, b)
    a + b
  end
end
```

Voer de tests opnieuw uit.

Uitvoer:

```
.

Voltooid in 0.002 seconden (bestanden duurden 0.08 seconden om te laden)
1 voorbeeld, 0 mislukkingen
```

## Diepgaande Duik

Testen in Ruby gaat terug tot Test::Unit, maar RSpec, geïntroduceerd in 2005, heeft het testen in Ruby met "behavior-driven development" gerevolutioneerd. Alternatieven voor RSpec zijn Minitest en Test::Unit. RSpec focust op leesbaarheid en de zakelijke kant; Minitest is minimalistischer en sneller. Typisch bootsen tests softwaregebruik na, controleren functies, data en randgevallen. Voor bestaande projecten, begin met het testen van de meest cruciale delen.

## Zie Ook

- RSpec GitHub: [github.com/rspec/rspec](https://github.com/rspec/rspec)
- Minitest: [rubygems.org/gems/minitest](https://rubygems.org/gems/minitest)
- "Effectief Testen met RSpec 3": Lees voor meer over RSpec principes en patronen.
