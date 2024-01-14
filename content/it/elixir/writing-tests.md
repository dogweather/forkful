---
title:                "Elixir: Scrivere test"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in Elixir
Scrivere test è un'ottima pratica per assicurarsi che il proprio codice funzioni correttamente. Inoltre, i test possono aiutare a individuare eventuali bug o errori nel codice prima che venga utilizzato in produzione.

## Come scrivere test in Elixir
Scrivere test in Elixir è molto semplice grazie alla sua sintassi pulita e alla sua capacità di gestire il concetto di "pura funzione". Iniziamo con un semplice esempio di test che verifica se una funzione restituisce il risultato corretto.

```elixir
defmodule Test do
  use ExUnit.Case

  test "somma corretta" do
    assert Sum.sum(1, 2) == 3
  end
end

```

In questo esempio, stiamo utilizzando il modulo integrato "ExUnit" per scrivere il nostro test. Utilizziamo il costrutto "defmodule" per definire un modulo chiamato "Test". All'interno del modulo, utilizziamo il costrutto "use ExUnit.Case" per includere il modulo ExUnit nelle nostre funzioni di test. Infine, utilizziamo il costrutto "test" per definire il nostro test, che controlla se la funzione "sum" del modulo "Sum" restituisce il risultato corretto. Nel caso in cui la somma sia corretta, il test passerà senza errori.

## Approfondimento sui test
Scrivere test in Elixir è estremamente importante per garantire la qualità del nostro codice. Oltre a verificare che il codice funzioni correttamente, i test ci aiutano a identificare eventuali bug o errori che potrebbero essere passati inosservati durante la fase di sviluppo. Inoltre, utilizzando la funzione `assert`, possiamo scrivere test più complessi che verificano una serie di risultati diversi per una determinata funzione.

Inoltre, Elixir offre una serie di librerie per aiutare a scrivere test più avanzati, come "ExMachina" per la generazione di dati falsi e "ExCoverage" per valutare la copertura dei test del nostro codice.

## Vedi anche
- [Documentazione ufficiale sui test in Elixir](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School: Writing Tests](https://elixirschool.com/it/lessons/basics/testing/)
- [GitBook: Testing Phoenix Applications](https://elixir-lang.gitbook.io/testing-phoenix/)