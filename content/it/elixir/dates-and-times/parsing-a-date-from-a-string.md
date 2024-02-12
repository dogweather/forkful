---
title:                "Analisi di una data da una stringa"
aliases: - /it/elixir/parsing-a-date-from-a-string.md
date:                  2024-01-28T02:05:09.518663-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisi di una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

L'analisi di una data da una stringa consiste nel prendere del testo, come "2023-04-05", e convertirlo in un formato data che il tuo programma può comprendere e con cui può lavorare. I programmatori lo fanno perché le date si presentano in molti formati e hanno bisogno di coerenza per confrontarle, ordinarle o memorizzarle correttamente.

## Come fare:

In Elixir, puoi analizzare le date utilizzando il modulo `Date`. Ecco come trasformare una stringa in una data:

```elixir
date_string = "2023-04-05"
{:ok, date} = Date.from_iso8601(date_string)
IO.inspect(date)
```

Esempio di output:

```elixir
~D[2023-04-05]
```

Per gestire formati diversi, puoi usare la libreria `Timex`:

```elixir
{:ok, datetime} = Timex.parse("05-04-2023", "{D}-{0M}-{YYYY}")
IO.inspect(datetime)
```

Esempio di output:

```elixir
#DateTime<2023-04-05 00:00:00Z>
```

## Approfondimento

La funzione `Date.from_iso8601/1` fa parte della libreria standard di Elixir, introdotta per garantire un parsing semplice dello standard di data ISO8601 - un formato di data comune. Ma la vita non è così semplice; le date si presentano in tantissimi formati. Qui entra in gioco `Timex`, una libreria Elixir di terze parti. È più ricca delle funzioni di data integrate di Elixir e aiuta a gestire una vasta varietà di formati di date.

Elixir stesso è immutabile, il che significa che anche le date analizzate non fanno eccezione; una volta create, non possono essere modificate. Questa caratteristica si ricollega alle radici della programmazione funzionale di Elixir, garantendo prevedibilità e una più facile ricerca degli errori.

Storicamente, l'analisi delle date è stata difficile a causa di standard variabili. Eppure, con libreria come `Timex` e le funzionalità del linguaggio in Elixir, la complessità viene astratta, rendendo la vita di uno sviluppatore un po' più semplice.

## Vedi Anche

- [Elixir Date](https://hexdocs.pm/elixir/Date.html)
- [Documentazione di Timex](https://hexdocs.pm/timex/Timex.html)
- [Standard ISO8601](https://www.iso.org/iso-8601-date-and-time-format.html)
