---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tests sind Skripte, die deinen Code automatisch überprüfen. Sie helfen dabei, Fehler schnell zu entdecken und sorgen für stabilen Code während der Entwicklung und danach.

## How to:
Ruby verwendet oft die `RSpec`-Bibliothek für Tests. Hier ist ein einfaches Beispiel für einen Test einer Methode `add`, die zwei Zahlen addiert:

```Ruby
require 'rspec'

def add(a, b)
  a + b
end

describe "Addition" do
  it "adds two numbers correctly" do
    expect(add(2, 3)).to eq(5)
  end
end
```

Ausführen der Tests:
```Ruby
$ rspec example_spec.rb
```

Erwartete Ausgabe:
```
.

Finished in 0.002 seconds (files took 0.08 seconds to load)
1 example, 0 failures
```

## Deep Dive
Tests in Ruby begannen mit dem Unit-Test-Framework `Test::Unit`, wurden aber durch `RSpec` populär, das eine BDD (Behavior-Driven Development) Syntax bietet. Alternativen zu `RSpec` sind unter anderem `Minitest` und `Cucumber`. Integrationstests mit Ruby on Rails werden oft mit `Capybara` durchgeführt. `RSpec` arbeitet durch Vergleich des tatsächlichen Ergebnisses (z.B. das Resultat einer Methode) mit dem erwarteten Ergebnis; bei Übereinstimmung ist der Test erfolgreich.

## See Also
- RSpec Dokumentation: https://rspec.info/documentation/
- Minitest: https://rubygems.org/gems/minitest
- Ruby Testing Best Practices: https://www.rubyguides.com/2019/03/ruby-testing/
- Capybara: https://github.com/teamcapybara/capybara
