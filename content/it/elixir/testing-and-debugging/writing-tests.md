---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:33.401052-07:00
description: "Scrivere test in Elixir comporta la creazione di script automatizzati\
  \ per convalidare il comportamento del tuo codice. I programmatori lo fanno per\u2026"
lastmod: '2024-03-13T22:44:43.086948-06:00'
model: gpt-4-0125-preview
summary: Scrivere test in Elixir comporta la creazione di script automatizzati per
  convalidare il comportamento del tuo codice.
title: Scrivere test
weight: 36
---

## Come fare:
Elixir utilizza ExUnit come framework di test incorporato, che è estremamente potente e facile da usare. Ecco un esempio di base:

1. Crea un nuovo file di test nella directory `test` del tuo progetto Elixir. Ad esempio, se stai testando un modulo chiamato `MathOperations`, il tuo file di test potrebbe essere `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Questo è un semplice caso di test per verificare la funzione di addizione
  test "l'addizione di due numeri" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Per eseguire i tuoi test, usa il comando `mix test` nel tuo terminale. Se la funzione `MathOperations.add/2` aggiunge correttamente due numeri, vedrai un output simile a:

```
..

Completato in 0.03 secondi
1 test, 0 fallimenti
```

Per i test che coinvolgono servizi esterni o API, potresti voler utilizzare librerie di mock, come `mox`, per evitare di colpire i servizi reali:

1. Aggiungi `mox` alle tue dipendenze in `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # altre dipendenze...
  ]
end
```

2. Definisci un modulo mock nel tuo helper di test (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, per: HTTPClientBehaviour)
```

3. Usa il mock nel tuo caso di test:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Questo indica a Mox di verificare che questo mock sia stato chiamato come previsto
  setup :verify_on_exit!

  test "ottiene dati dall'API" do
    # Imposta la risposta del mock
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Risposta simulata"} end)
    
    assert SomeAPIClient.get_data() == "Risposta simulata"
  end
end
```

Quando esegui `mix test`, questo setup ti permette di isolare i tuoi test unitari dalle vere dipendenze esterne, concentrandoti sul comportamento del tuo codice. Questo modello assicura che i tuoi test vengano eseguiti rapidamente e rimangano affidabili, indipendentemente dallo stato del servizio esterno o dalla connettività internet.
