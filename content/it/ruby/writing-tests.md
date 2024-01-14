---
title:                "Ruby: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è una pratica fondamentale per garantire la qualità del codice e per facilitare la manutenzione a lungo termine. I test aiutano ad identificare eventuali bug e a prevenire regressioni del codice. Inoltre, sono uno strumento importante per la collaborazione tra i membri del team ed assicurano che ogni funzionalità del codice funzioni correttamente.

## Come

Per scrivere test in Ruby, è necessario utilizzare un framework di testing come RSpec o MiniTest. Di seguito è riportato un esempio di codice di test che verifica se il metodo "sum" di una classe "Calculator" restituisce correttamente la somma di due numeri:

```Ruby
require 'minitest/autorun'

class CalculatorTest < MiniTest::Test
  def setup
    @calculator = Calculator.new
  end

  def test_sum_method
    assert_equal 4, @calculator.sum(2, 2)
  end
end
```

L'output di questo test dovrebbe essere "PASS" poiché il valore atteso (4) è uguale al valore effettivo restituito dal metodo sum del calcolatore.

## Approfondimento

Scrivere test efficaci richiede di comprendere il concetto di "Test-driven development" (TDD). Questo approccio prevede di scrivere i test prima di scrivere il codice effettivo, in modo da guidare il processo di sviluppo. In questo modo, ci si assicura che solo il codice necessario venga scritto per soddisfare i requisiti del test. Inoltre, TDD aiuta a mantenere un codice più modulare e facilmente testabile.

## Vedi anche

- [RSpec Documentation](https://rspec.info/)
- [MiniTest Documentation](https://guides.rubyonrails.org/testing.html)
- [The Rspec Book](https://www.amazon.com/Rspec-Book-Behaviour-Development-Ruby-ebook/dp/B018PO7IUW)