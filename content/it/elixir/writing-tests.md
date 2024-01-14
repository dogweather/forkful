---
title:                "Elixir: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è un aspetto importante della programmazione in Elixir. I test ci permettono di verificare che il nostro codice funzioni correttamente e ci aiutano a trovare eventuali bug prima che essi possano causare problemi in produzione. Inoltre, i test ci danno una maggiore fiducia nel nostro codice e ci permettono di effettuare modifiche in modo più sicuro.

## Come fare

Per scrivere test in Elixir, dobbiamo utilizzare il modulo `ExUnit` che fa parte della libreria standard della linguaggio. Possiamo definire una funzione di test utilizzando l'attributo `test` e fornendo una descrizione dell'esito atteso. Ad esempio:

```Elixir
test "la somma di 2 e 2 dovrebbe essere 4" do
  assert 2 + 2 == 4
end
```

Possiamo eseguire i nostri test utilizzando il comando `mix test` dalla radice del nostro progetto. Se tutti i test passano, dovremmo ottenere un output simile a questo:

```
ExUnit.run
.
.

Finished in 0.04 seconds
2 tests, 0 failures
```

Dobbiamo assicurarci anche di testare i casi di errore, per esempio con l'attributo `test` possiamo fornire una descrizione del caso di errore atteso. Ad esempio:

```Elixir
test "la radice quadrata di -1 dovrebbe dare un errore" do
  assert raises ArithmeticError, fn -> :math.sqrt(-1) end
end
```

## Approfondimenti

Scrivere test di unità è solo una parte dei test disponibili in Elixir. Possiamo anche scrivere test di integrazione utilizzando il modulo `Phoenix.ConnTest` per testare la nostra applicazione web. Possiamo anche utilizzare `ExUnit` per testare il nostro codice asincrono e i processi paralleli.

## Vedi anche

- [Documentazione ufficiale di ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Guida alla programmazione in Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Tutorial su come scrivere test in Elixir](https://pragmaticstudio.com/tutorials/writing-elixir-tests)