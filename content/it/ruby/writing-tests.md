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

## Perché

Scrivere codice di qualità è fondamentale per qualsiasi programmatore. I test sono uno strumento essenziale per garantire che il nostro codice funzioni correttamente e continui a farlo nel futuro. Facilitano anche la comprensione e la manutenzione del codice.

## Come fare

Per scrivere test efficaci, dobbiamo utilizzare il framework di testing integrato di Ruby, chiamato Minitest. Iniziamo creando un nuovo file per i nostri test e utilizzando l'istruzione `require` per aggiungere Minitest al nostro progetto:

```Ruby
require "minitest/autorun"
```

Successivamente, creiamo una classe che eredita dalla classe `Minitest::Test` e definiamo i nostri test all'interno di metodi che iniziano con `test_`:

```Ruby
class CalculatorTest < Minitest::Test
  def test_add
    # codice per testare la funzione della calcolatrice per l'addizione
  end

  def test_subtract
    # codice per testare la funzione della calcolatrice per la sottrazione
  end
end
```

Utilizziamo gli assert di Minitest per verificare se il nostro codice produce il risultato previsto:

```Ruby
assert_equal 4, calculator.add(2, 2)
assert_equal 6, calculator.subtract(8, 2)
```

Una volta che i nostri test sono pronti, possiamo eseguirli dal terminale utilizzando il comando `ruby` seguito dal nome del nostro file dei test:

```
ruby calculator_test.rb
```

Se tutto funziona correttamente, dovremmo vedere un output simile al seguente:

```
Run options: --seed 29162

# Running:

..

Finished in 0.002154s, 927.4375 runs/s, 1854.8750 assertions/s.

2 runs, 4 assertions, 0 failures, 0 errors, 0 skips
```

Ora possiamo continuare a sviluppare il nostro codice, avendo la sicurezza che se qualcosa smetterà di funzionare in futuro, il nostro test ci avviserà.

## Approfondimento

Scrivere test efficaci richiede un equilibrio tra copertura e affidabilità. Dobbiamo essere sicuri di coprire tutti i casi possibili senza cadere nella trappola di testare ogni singola riga di codice. Inoltre, è importante seguire le best practice di Ruby, come utilizzare metodi di test brevi e descrittivi e non utilizzare test come mezzo per controllare il flusso del nostro codice.

## Vedi anche

- [La guida ufficiale a Minitest](https://github.com/seattlerb/minitest#minitest---miniature-test-framework) 
- [Test Driven Development (TDD)](https://www.agilealliance.org/glossary/tdd/) 
- [Ruby Best Practices](https://github.com/ruby/ruby/blob/trunk/README.md)