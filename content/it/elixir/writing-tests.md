---
title:    "Elixir: Scrivere test"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è un aspetto fondamentale della programmazione di Elixir, in quanto permette di verificare la correttezza del codice e di prevenire errori futuri. Inoltre, aiuta a mantenere una maggiore qualità del software e a facilitare la collaborazione all'interno del team di sviluppo.

## Come Fare

Per scrivere test in Elixir, è necessario utilizzare il modulo di test nativo della libreria standard, chiamato "ExUnit". Inizialmente, è necessario importare il modulo nel file di test, utilizzando il comando `use ExUnit.Case` all'interno di una definizione di modulo.

Di seguito, è possibile creare una funzione di test utilizzando la macro `test/2`, passando come primo parametro una descrizione del test e come secondo parametro una funzione anonima contenente il codice del test. Ad esempio:

```Elixir
test "la somma di due numeri" do
  assert 2 + 2 == 4
end
```

Per eseguire il test, è sufficiente chiamare il comando `mix test` da linea di comando, che rileverà tutti i file di test presenti nella directory `test` e li eseguirà.

Inoltre, è possibile utilizzare le macro `assert` e `refute` per definire asserzioni specifiche di un test e verificare se un'asserzione è vera o falsa. Ad esempio:

```Elixir
assert 10 > 5
refute 5 > 10
```

## Approfondimento

Esistono diverse pratiche e tecniche per scrivere test efficaci in Elixir. Una di queste è la separazione del codice di produzione dal codice di test, che consente di mantenere una maggiore leggibilità e facilita il debugging.

Inoltre, è buona pratica utilizzare il TDD (Test Driven Development), ovvero scrivere i test prima di scrivere il codice di produzione. In questo modo, si garantisce che il codice scritto corrisponda alle aspettative dei test e che il software funzioni correttamente.

Infine, è importante testare diversi scenari e casi limite per garantire un'adeguata copertura dei test e rilevare eventuali errori o anomalie.

## Vedi Anche

- [Documentazione ufficiale di ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - Testing](https://elixirschool.com/it/lessons/basics/testing/)
- [Articolo sul TDD in Elixir](https://beam-wisdoms.clau.se/en/latest/testing/tdd/the_absolute_minimum_of_tdd_in_elixir.html)