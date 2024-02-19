---
aliases:
- /it/elixir/logging/
date: 2024-01-26 01:02:22.069138-07:00
description: "Il logging nello sviluppo software \xE8 la tecnica di registrazione\
  \ degli eventi che si verificano mentre un programma \xE8 in esecuzione, tipicamente\
  \ su un\u2026"
lastmod: 2024-02-18 23:08:55.606614
model: gpt-4-1106-preview
summary: "Il logging nello sviluppo software \xE8 la tecnica di registrazione degli\
  \ eventi che si verificano mentre un programma \xE8 in esecuzione, tipicamente su\
  \ un\u2026"
title: Registrazione Eventi (Logging)
---

{{< edit_this_page >}}

## Cos'è & Perché?
Il logging nello sviluppo software è la tecnica di registrazione degli eventi che si verificano mentre un programma è in esecuzione, tipicamente su un file o un sistema esterno. I programmatori lo fanno per ottenere informazioni sul comportamento del software, risolvere problemi e mantenere un registro della cronologia operativa, che è fondamentale per il debug e il monitoraggio dello stato di salute delle applicazioni.

## Come fare:
In Elixir, il modo principale per registrare le informazioni è attraverso il modulo incorporato `Logger`. Ecco come puoi utilizzarlo:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Avvio processo importante con parametro: #{param}")

    # Simula il lavoro in corso
    :timer.sleep(1000)

    Logger.debug("Processo completato.")
  rescue
    error -> Logger.error("Si è verificato un errore: #{inspect(error)}")
  end
end

# Per vedere i tuoi log, basta chiamare la funzione:
MyApplication.do_something_important("MyParam")
```

Questo semplice frammento mostra come registrare informazioni a diversi livelli (`info`, `debug` ed `error`). Quando esegui questo codice, non vedrai il messaggio di debug a meno che tu non configurare il livello di Logger su `:debug`. Per impostazione predefinita, il Logger di Elixir filtra i messaggi di log al di sotto del livello `:info`.

Un esempio di output al livello `:info` potrebbe apparire così:
```
14:32:40.123 [info]  Avvio processo importante con parametro: MyParam
14:32:41.126 [error] Si è verificato un errore: %RuntimeError{message: "errore di runtime"}
```

## Approfondimento:
Il `Logger` di Elixir è uno strumento incorporato che fa parte del linguaggio fin dai suoi primi giorni. È influenzato dai sistemi di logging di altri linguaggi BEAM come Erlang. Il logger offre diversi livelli di registrazione – `:debug`, `:info`, `:warn` ed `:error` – ed è pluggabile, consentendo di collegare diversi backend per la gestione dei messaggi di log.

Un'alternativa al Logger incorporato per scenari più complessi è l'uso di librerie di logging come `Logstash` o `Sentry` per Elixir, che possono offrire funzionalità aggiuntive come il tracciamento degli errori e l'aggregazione in un formato più visuale. Per lo sviluppo locale, gli sviluppatori Elixir si affidano spesso alla funzionalità del Logger incorporato per la sua semplicità e integrazione con la BEAM VM.

Sotto il cofano, il modulo Logger offre logging asincrono e sincrono. Il logging asincrono, che è quello predefinito, non blocca l'esecuzione della tua applicazione durante la registrazione dei messaggi. Questo assicura che il logging non influenzi negativamente le prestazioni. Tuttavia, il logging sincrono può essere abilitato per casi in cui è necessario garantire che i messaggi vengano registrati nell'ordine in cui sono stati inviati.

La configurazione del Logger può essere regolata nel file `config/config.exs` di un'applicazione Elixir, dove puoi impostare il livello di registrazione, il formato, i metadati e altro. Ricorda sempre di regolare i tuoi livelli di registrazione e gli output per i diversi ambienti; non vorresti che log di debug verbosi invadano i tuoi sistemi di produzione.

## Vedi Anche:
- La documentazione ufficiale del Logger Elixir: https://hexdocs.pm/logger/Logger.html
- Un articolo di blog sulle migliori pratiche di logging in Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry per Elixir su Hex: https://hex.pm/packages/sentry
- La lezione di Elixir School sul Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
