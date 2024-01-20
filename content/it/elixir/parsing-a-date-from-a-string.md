---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il parsing di una data da una stringa in Elixir, come in molti linguaggi di programmazione, è il processo di trasformazione di una stringa che rappresenta una data in un formato leggibile e utilizzabile dalla macchina. Questa operazione è importantissima quando si deve manipolare, comparare o calcolare date usando dati immessi come stringhe o provenienti da file di log o database.

## Come fare:

Per fare il parsing di una data da una stringa in Elixir potrete usare la funzione `Date.from_iso8601/2`. Questa funzione esegue il parsing di una stringa nel formato ISO 8601 (ad esempio "2022-02-21") e la converte in una struttura `Date`.

```elixir
string_date = "2022-02-21" 
{:ok, date} = Date.from_iso8601(string_date)
```

L'output di quel frammento di codice sarà:

```elixir
{:ok, ~D[2022-02-21]}
```

## Approfondimenti

Elixir non ha sempre fornito il supporto per la gestione delle date. Solo con la versione 1.3 del 2016, Elixir ha introdotto il modulo `Date` all'interno della sua libreria standard.

Esistono diverse alternative per fare il parsing di una data da una stringa. Ad esempio, possiamo utilizzare la libreria parecchio popolare Timex. Questa libreria offre una serie di funzionalità addizionali, compresa la possibilità di fare il parsing di date in diversi formati di stringhe, non solo ISO 8601.

Per quanto riguarda i dettagli dell'implementazione, la funzione `Date.from_iso8601/2` esegue il parsing di una stringa e la converte in una tupla contenente `:ok` e il risultato, o `:error` e il motivo dell'errore, se la stringa non può essere convertita.

## Vedere Anche:

- Documentazione di Elixir sul modulo [`Date`](https://hexdocs.pm/elixir/Date.html)
- Conoscere meglio la libreria [`Timex`](https://hexdocs.pm/timex/readme.html)
- Specifiche della ISO sul formato [`ISO 8601`](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Elixir School](https://elixirschool.com/en/lessons/basics/date_and_time/) per lezioni pratiche riguardanti la manipolazione di date e tempo in Elixir.