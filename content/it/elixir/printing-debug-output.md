---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Che cosa e Perche?

Stampare l'output del debug è un processo di tracciamento dei dati di un programma mentre viene eseguito. I programmatori lo fanno per risolvere i problemi e per capire meglio come funziona il loro codice.

## Come fare:

Ecco un esempio di come potresti stampare l'output di debug in Elixir:

```elixir
IO.inspect("Hello, World!")
```

Questo stamperà "Hello, World!" sulla console. 

E un altro esempio dove usiamo un modulo e una funzione:

```elixir
defmodule DebugEsempio do
  def debug_output(data) do
    IO.inspect(data)
  end
end

DebugEsempio.debug_output("Ciao, Mondo!")
```
Anche questo stamperà "Ciao, Mondo!" sulla console.

## Approfondimento:

La stampa dell'output di debug è una tecnica che risale alle origini della programmazione. In Elixir, `IO.inspect` è il modo più comune per ottenerlo, ma ci sono anche alternative come `IO.puts` o l'utilizzo del modulo `Logger`.

`IO.inspect` restituisce l'argomento che viene passato senza modificarlo, rendendolo ideale per il debug. `IO.puts`, d'altra parte, restituisce `:ok` e stampa l'argomento, il che può portare a comportamenti indesiderati se non si è consapevoli di questa distinzione.

Elixir fornisce anche un modulo `Logger` che può essere utilizzato per tracciare messaggi di debug in modo più strutturato. Con `Logger`, possiamo configurare il livello di log (per esempio, :debug, :info, :warn, :error), e decidere dove inviare i messaggi di log basandoci su questa configurazione.

```elixir
Logger.debug("Questo è un messaggio di debug")
Logger.info("Questo è un messaggio informativo")
Logger.warn("Questo è un messaggio di avviso")
Logger.error("Questo è un messaggio di errore")
```

Ricorda che la stampa dell'output del debug può influenzare le prestazioni del tuo codice, quindi dovresti cercare di utilizzarla il meno possibile in produzione.

## Vedi Anche:

[Elixir School: Basic debugging](https://elixirschool.com/en/lessons/specifics/debugging/) - Una guida alla debug in Elixir.

[Elixir: `IO` module documentation](https://hexdocs.pm/elixir/IO.html) - Documentazione ufficiale del modulo `IO`.

[Elixir: `Logger` module documentation](https://hexdocs.pm/elixir/Logger.html) - Documentazione ufficiale del modulo `Logger`.