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

Cosa sono i test e perché i programmatori li scrivono?

Scrivere test è il processo di creare codice che verifica che il nostro software funzioni come ci aspettiamo. I programmatori lo fanno per essere sicuri che il nostro codice sia affidabile e funzioni correttamente, identificando eventuali errori o bug prima che il software venga rilasciato agli utenti.

Come: Esempi di codice e output di esempio

```Elixir
defmodule Calculator do
  def add(x, y) do
  	x + y
  end

  def subtract(x, y) do
  	x - y
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "addition" do
    assert Calculator.add(2, 2) == 4
  end

  test "subtraction" do
    assert Calculator.subtract(5, 2) == 3
  end
end
```

Output di esempio:

```sh
$ mix test
Compiling 2 files (.ex)
...

Finished in 0.03 seconds
2 tests, 0 failures
```

Approfondimento: Contesto storico, alternative e dettagli di implementazione sui test

Scrivere test è una pratica comune nel processo di sviluppo del software. Sebbene non sia obbligatoria, è considerata una best practice per garantire la qualità del codice. Alcune alternative al testing unitario sono il testing di integrazione e il testing funzionale, che coprono rispettivamente la gestione delle interfacce tra le diverse parti del software e i casi d'uso completi.

Un modo comune per scrivere test in Elixir è utilizzare il modulo ```ExUnit.Case```, che fornisce una serie di macro per definire test e assert personalizzati. Inoltre, Elixir offre anche il supporto per i test generativi con il modulo ```StreamData```, che genera dati casuali per testare il nostro codice in modo più approfondito.

Vedi anche: Link correlati

- [Documentazione su testing in Elixir](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-mocks.html)
- [Articolo su come scrivere test di integrazione in Elixir](https://thoughtbot.com/blog/testing-and-dumping-external-http-apis-in-elixir)
- [Esempi di test generativi in Elixir](https://elixirschool.com/en/lessons/advanced/stream-data/)