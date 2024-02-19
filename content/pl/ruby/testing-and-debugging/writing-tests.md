---
aliases:
- /pl/ruby/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:09.934883-07:00
description: "Testowanie w Ruby polega na weryfikacji, czy Tw\xF3j kod zachowuje si\u0119\
  \ zgodnie z oczekiwaniami w r\xF3\u017Cnych warunkach. Programi\u015Bci pisz\u0105\
  \ testy, aby zapewni\u0107\u2026"
lastmod: 2024-02-18 23:08:50.129128
model: gpt-4-0125-preview
summary: "Testowanie w Ruby polega na weryfikacji, czy Tw\xF3j kod zachowuje si\u0119\
  \ zgodnie z oczekiwaniami w r\xF3\u017Cnych warunkach. Programi\u015Bci pisz\u0105\
  \ testy, aby zapewni\u0107\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Testowanie w Ruby polega na weryfikacji, czy Twój kod zachowuje się zgodnie z oczekiwaniami w różnych warunkach. Programiści piszą testy, aby zapewnić poprawność, zapobiegać regresji i ułatwić refaktoryzację, dążąc do stworzenia solidnych i łatwych w utrzymaniu aplikacji.

## Jak to zrobić:
Ruby jest wyposażony w wbudowaną bibliotekę o nazwie `Test::Unit` do pisania testów jednostkowych, zawierającą praktyki testowe w prostych strukturach. Jednak społeczność Ruby często skłania się ku bibliotekom stron trzecich, takim jak RSpec i Minitest, ze względu na ich zwiększoną ekspresywność i elastyczność.

### Używanie `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Uruchom swój plik testowy z terminala, a powinieneś otrzymać wynik wskazujący na sukces lub niepowodzenie testów:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Używanie RSpec:
RSpec jest popularnym frameworkiem BDD (Behavior-Driven Development) dla Ruby. Zainstaluj gem za pomocą `gem install rspec`, a następnie zainicjalizuj go w swoim projekcie za pomocą `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'poprawnie dodaje dwie liczby' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Uruchom testy za pomocą polecenia `rspec`. Przykładowy wynik:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Używanie Minitest:
Minitest oferuje kompletny zestaw narzędzi do testowania wspierających TDD, BDD, mocking i benchmarking. Zainstaluj go za pomocą `gem install minitest` i używaj w następujący sposób:

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

Uruchom swój plik testowy bezpośrednio lub przez zadanie `rake` ustawione dla minitest. Przykładowy wynik:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Implementując testy w swoich projektach Ruby z użyciem tych bibliotek, stosujesz najlepsze praktyki, co prowadzi do bardziej niezawodnych i łatwiejszych w utrzymaniu baz kodów.
