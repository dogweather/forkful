---
title:                "Skriva tester"
aliases: - /sv/ruby/writing-tests.md
date:                  2024-02-03T19:31:51.719481-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Testning i Ruby handlar om att verifiera att din kod beter sig som förväntat under olika förhållanden. Programerare skriver tester för att säkerställa korrekthet, förhindra regressioner och underlätta refaktorisering, med målet att skapa robusta och underhållbara applikationer.

## Hur man gör:
Ruby kommer med ett inbyggt bibliotek som heter `Test::Unit` för att skriva enhetstester, vilket inkapslar testpraxis inom enkel struktur. Dock lutar sig Ruby-gemenskapen ofta mot tredjepartsbibliotek som RSpec och Minitest på grund av deras ökade uttrycksfullhet och flexibilitet.

### Använda `Test::Unit`:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Kör din testfil från terminalen, och du bör få en utskrift som indikerar framgång eller misslyckande med testerna:
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### Använda RSpec:
RSpec är ett populärt BDD (Behavior-Driven Development) ramverk för Ruby. Installera gemmet med `gem install rspec`, sedan initiera det i ditt projekt med `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'lägger korrekt ihop två nummer' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Kör tester med kommandot `rspec`. Exempel på utskrift:
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Använda Minitest:
Minitest erbjuder ett komplett paket av testmöjligheter som stödjer TDD, BDD, mocking och benchmarking. Installera det med `gem install minitest` och använd enligt följande:

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

Kör din testfil direkt eller genom den `rake`-uppgift som är inställd för minitest. Exempel på utskrift:
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Genom att implementera tester i dina Ruby-projekt med hjälp av dessa bibliotek, följer du bästa praxis, vilket leder till mer pålitliga och underhållbara kodbasar.
