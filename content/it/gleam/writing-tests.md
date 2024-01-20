---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Che cosa e Perché?
Scrivere test è il processo di creazione di codice per verificare automaticamente che il codice della tua applicazione si comporti come previsto. Gli sviluppatori lo fanno per prevenire bug, assicurare qualità e rendere il refactoring sicuro.

## Come Fare:
```Gleam
import gleam/should
import my_module

pub fn my_test() {
  // Definisci il test aspettandoti un risultato
  should.equal(my_module.my_function(), "risultato_atteso")
}

// Eseguire `gleam test` per avviare i test e ricevere output simile a questo:
// test my_test ... ok
```

## Approfondimento
Gleam è un linguaggio staticamente tipizzato per la BEAM, l'ambiente di esecuzione di Erlang. Si ispira a Rust e Elm per ottenere sicurezza e piacere nella scrittura. A differenza di Elixir ed Erlang, che usano ExUnit ed EUnit per i test, Gleam usa il proprio framework di testing integrato, offrendo un'esperienza più coesa. Gli sviluppatori possono usare `gleam/should` per un'interfaccia chiara e descriptiva per l'asserzione.

## Vedere Anche:
- Documentazione Gleam: https://gleam.run
- Libreria `gleam/should`: https://hexdocs.pm/gleam_stdlib/gleam/should/
- BEAM: https://erlang.org/doc/apps/erts/erlang_vm.html
- Articolo su Test Driven Development (TDD): https://martinfowler.com/bliki/TestDrivenDevelopment.html