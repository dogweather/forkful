---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:53.464772-07:00
description: "Das Testen in Ruby dient dazu, zu verifizieren, dass Ihr Code unter\
  \ verschiedenen Bedingungen wie erwartet funktioniert. Programmierer erstellen Tests,\
  \ um\u2026"
lastmod: '2024-02-25T18:49:51.450718-07:00'
model: gpt-4-0125-preview
summary: "Das Testen in Ruby dient dazu, zu verifizieren, dass Ihr Code unter verschiedenen\
  \ Bedingungen wie erwartet funktioniert. Programmierer erstellen Tests, um\u2026"
title: Tests Schreiben
---

{{< edit_this_page >}}

## Was & Warum?
Das Testen in Ruby dient dazu, zu verifizieren, dass Ihr Code unter verschiedenen Bedingungen wie erwartet funktioniert. Programmierer erstellen Tests, um die Korrektheit sicherzustellen, Regressionen zu verhindern und das Refactoring zu erleichtern, mit dem Ziel, robuste und wartbare Anwendungen zu erreichen.

## Wie:
Ruby verfügt über eine eingebaute Bibliothek namens `Test::Unit` für das Schreiben von Unit-Tests, die Testpraktiken in übersichtlichen Strukturen kapseln. Die Ruby-Gemeinschaft neigt jedoch oft zu Drittbibliotheken wie RSpec und Minitest aufgrund ihrer gesteigerten Ausdrucksfähigkeit und Flexibilität.

### Verwendung von `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Führen Sie Ihre Testdatei über das Terminal aus, und Sie sollten eine Ausgabe erhalten, die Erfolg oder Misserfolg der Tests anzeigt:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Verwendung von RSpec:
RSpec ist ein beliebtes BDD (Behavior-Driven Development) Framework für Ruby. Installieren Sie das Gem mit `gem install rspec`, dann initialisieren Sie es in Ihrem Projekt mit `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'addiert zwei Zahlen korrekt' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Führen Sie Tests mit dem Befehl `rspec` aus. Beispiel-Ausgabe:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Verwendung von Minitest:
Minitest bietet eine vollständige Palette an Testmöglichkeiten, die TDD, BDD, Mocking und Benchmarking unterstützen. Installieren Sie es mit `gem install minitest` und verwenden Sie es wie folgt:

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

Führen Sie Ihre Testdatei direkt oder über die `rake`-Aufgabe, die für minitest eingerichtet wurde, aus. Beispiel-Ausgabe:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Indem Sie Tests in Ihren Ruby-Projekten mit diesen Bibliotheken implementieren, halten Sie sich an die Best Practices, was zu zuverlässigeren und wartbareren Codebasen führt.
