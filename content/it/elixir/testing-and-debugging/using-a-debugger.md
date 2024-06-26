---
date: 2024-01-26 03:48:21.142169-07:00
description: "Come fare: Elixir viene fornito con un debugger grafico integrato chiamato\
  \ `:debugger`. Per usarlo, \xE8 necessario avviarlo e collegarsi al processo in\u2026"
lastmod: '2024-03-13T22:44:43.087889-06:00'
model: gpt-4-0125-preview
summary: Elixir viene fornito con un debugger grafico integrato chiamato `:debugger`.
title: Utilizzo di un debugger
weight: 35
---

## Come fare:
Elixir viene fornito con un debugger grafico integrato chiamato `:debugger`. Per usarlo, è necessario avviarlo e collegarsi al processo in esecuzione.

Prima di tutto, assicurati di avere `:debugger` avviato all'interno di una sessione `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Ora, interpreta il modulo di codice che vuoi eseguire il debug:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Puoi impostare un punto di interruzione:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

E poi, esegui la tua funzione per raggiungere il punto di interruzione e procedere passo passo con il tuo codice:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Il debugger interromperà l'esecuzione alla riga con il punto di interruzione
```

## Approfondimento
Prima del `:debugger` di Elixir, Erlang forniva il debugger utilizzato da Elixir; è robusto e ottimo nel gestire processi concorrenti, uno dei punti di forza della VM di Erlang (BEAM). A differenza di alcuni altri debugger, `:debugger` non consente la modifica delle variabili al volo, a causa della natura immutabile dei dati in Elixir. Per quanto riguarda le alternative, esiste `IEx.pry` che consente di mettere in pausa l'esecuzione e saltare in un REPL in qualsiasi punto del codice, il che può essere estremamente utile.

Mentre `:debugger` è buono per un'interfaccia grafica, alcuni potrebbero preferire lo strumento integrato `:observer` che offre anche l'ispezione dei processi e metriche di sistema, anche se non specificamente indirizzato al passo passo attraverso il codice. La comunità di Elixir contribuisce anche con strumenti come `visualixir` e `rexbug`, espandendo l'ecosistema degli strumenti di debug oltre le opzioni predefinite.

## Vedi anche
- Guida ufficiale per iniziare con Elixir sul Debug: https://elixir-lang.org/getting-started/debugging.html
- Documentazione di Erlang su `:debugger`: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Discussioni sul Forum di Elixir sulle Tecniche di Debugging: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
