---
title:                "Scrivere test"
html_title:           "Elixir: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test?
Scrivere test è fondamentale per garantire che il codice che stiamo sviluppando sia robusto e funzionante. I test ci permettono di individuare eventuali errori o bug nel codice in modo tempestivo, aumentando la qualità del nostro prodotto finale.

## Come scrivere test in Elixir
Scrivere test in Elixir è un processo semplice e intuitivo. Utilizzando il modulo `ExUnit`, possiamo definire una suite di test utilizzando vari metodi come `assert`, `refute` e `match`, che ci consentono di verificare le nostre aspettative sulle funzioni che stiamo testando.

```Elixir
defmodule TestCalculator do
  use ExUnit.Case
  
  # Definiamo un test case per la funzione sum, che dovrebbe sommare i due numeri dati
  test "sum" do
    result = Calculator.sum(2, 3)
    assert result == 5
  end
end
```

In questo esempio, stiamo verificando se la funzione `Calculator.sum` restituisce il valore corretto quando gli passiamo i parametri 2 e 3. Oltre a testare la funzione, possiamo anche testare il comportamento in caso di errori o eccezioni:

```Elixir
# Testiamo la funzione divide per verificare se gestisce correttamente la divisione per 0
test "divide per 0" do
  assert_raise ZeroDivisionError, fn -> Calculator.divide(5, 0) end
end
```

Una volta definiti tutti i nostri test, possiamo eseguirli utilizzando il comando `mix test`. Questo ci fornirà un report dei test passati e di quelli falliti, con dettagli sulle asserzioni che non sono andate a buon fine.

## Approfondimento sulla scrittura dei test
In aggiunta alle asserzioni standard, possiamo anche utilizzare altre utili funzioni come `assert_receive`, `assert_raise` e `assert_raise_message` per testare il comportamento di funzioni che interagiscono con altri processi o lanciano eccezioni specifiche. Inoltre, possiamo utilizzare anche il modulo `ExUnit.CaptureIO` per testare l'output di funzioni che stampino sulla console.

## Vedi anche
- [ExUnit documentation](https://hexdocs.pm/ex_unit/ExUnit.html)
- [TDD - Test Driven Development](https://www.vervesearch.com/blog/tdd-test-driven-development/)
- [Unit Testing in Elixir](https://semaphoreci.com/community/tutorials/unit-testing-in-elixir)