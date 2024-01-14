---
title:    "Ruby: Scrivere test"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test è importante in Ruby

Quando si programma in Ruby, è fondamentale scrivere test per garantire che il codice funzioni correttamente e per prevenire eventuali bug. I test sono una parte essenziale dello sviluppo di software di qualità e possono aiutare a raggiungere una maggiore stabilità e affidabilità del codice.

## Come scrivere test in Ruby

Scrivere test in Ruby è incredibilmente semplice e può essere fatto utilizzando il framework di test integrato, chiamato MiniTest. Di seguito è riportato un esempio di come creare un test utilizzando MiniTest:

```Ruby
require "minitest/autorun" # importa il framework MiniTest

# Definisce una classe per il test
class TestSomma < MiniTest::Test
  # Definisce un metodo di test
  def test_somma
    assert_equal 8, 5 + 3 # verifica che 5 + 3 sia uguale a 8
  end
end
```

Una volta eseguito il test, si dovrebbe ottenere un output simile a questo:

```
Run options: --seed 49237

# Running:

.

Finished in 0.000920s, 1086.9565 runs/s, 1086.9565 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

L'output indica che il test è stato eseguito con successo e che non sono stati riscontrati problemi. Tuttavia, se si inserisce un valore diverso nel metodo di test, come ad esempio 4 invece di 3, si otterrà un output diverso:

```
Run options: --seed 49237

# Running:

F

Finished in 0.001015s, 985.2217 runs/s, 1970.4434 assertions/s.

  1) Failure:
TestSomma#test_somma [tests.rb:6]:
Expected: 8
  Actual: 9

1 runs, 2 assertions, 1 failures, 0 errors, 0 skips
```

Come si può notare, il test è fallito perché il risultato attuale è 9 invece di 8.

## Approfondimento sui test

I test non solo aiutano a identificare i bug nel codice, ma possono anche servire come documentazione. Scrivendo test accurati, è possibile capire meglio come funziona il codice e quali funzionalità sono state implementate. Inoltre, i test possono essere eseguiti ogni volta che si apportano modifiche al codice, in modo da assicurarsi che tutto funzioni ancora correttamente e per evitare di introdurre nuovi bug.

Un'altra pratica comune è quella di scrivere i test prima di scrivere il codice effettivo. Questo approccio, noto come "test-driven development", può aiutare a scrivere codice più pulito e ben strutturato.

## Vedi anche

- [MiniTest Gem](https://github.com/seattlerb/minitest)
- [Ruby in Twenty Minutes - Test](https://www.ruby-lang.org/en/documentation/quickstart/5/)

# Visualizza anche