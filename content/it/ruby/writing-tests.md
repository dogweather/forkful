---
title:                "Scrivere test"
date:                  2024-01-19
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test significa creare codice specifico per verificare che altri codici funzionino come previsto. I programmatori testano per prevenire bug, assicurare qualità, e facilitare aggiornamenti futuri.

## How to:
Ruby usa MiniTest e RSpec come framework di test. Ecco un esempio con MiniTest:

```Ruby
require 'minitest/autorun'

class CalcolatriceTest < Minitest::Test
  def setup
    @calc = Calcolatrice.new
  end

  def test_somma
    assert_equal 5, @calc.somma(2, 3)
  end
end

class Calcolatrice
  def somma(a, b)
    a + b
  end
end
```

Output atteso:

```
Run options: --seed 12345

# Running:

.

Finished in 0.001025s, 976.5625 runs/s, 976.5625 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

## Deep Dive
Il TDD (Test-Driven Development) guida lo sviluppo con test scritti prima del codice vero e proprio. MiniTest è integrato in Ruby, mentre RSpec è una gemma BDD (Behavior-Driven Development) che offre un DSL più leggibile. L'implementazione di test nei progetti Ruby è diventata norma per molti sviluppatori per la sua efficacia nel ridurre errori e fornire documentazione.

## See Also
- RSpec: [Guida ufficiale](https://rspec.info/)
- TDD/BDD: [Approfondimenti su TDD/BDD](https://martinfowler.com/bliki/TestDrivenDevelopment.html)
