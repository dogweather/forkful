---
title:                "Scrivere test"
html_title:           "Ruby: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Scrivere test è una pratica comune tra i programmatori per verificare che il codice funzioni correttamente e per identificare eventuali errori. I test consentono di risparmiare tempo nella fase di debugging e di garantire la qualità del codice.

## Come fare:
Ecco un esempio di come scrivere un test in Ruby utilizzando la libreria standard `Test::Unit`:

```Ruby
  require 'test/unit'
  
  class TestString < Test::Unit::TestCase
    def test_length
      assert_equal(5, 'Hello'.length)
    end
  end
```

L'output dovrebbe essere `1 runs, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 notifications`.

## Approfondimento:
La pratica di scrivere test è diventata sempre più popolare negli ultimi anni grazie all'approccio di sviluppo noto come "test-driven development" (TDD). Alcune alternative a `Test::Unit` includono `RSpec` e `minitest`.

Per implementare i test, è importante comprendere i concetti di "assertions" e "fixtures". Le assertions sono dichiarazioni che descrivono i risultati attesi del test, mentre le fixtures sono dati di prova utilizzati per eseguire i test.

## Vedi anche: