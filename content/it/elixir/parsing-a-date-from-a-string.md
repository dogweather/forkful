---
title:                "Estrapolare una data da una stringa"
html_title:           "Elixir: Estrapolare una data da una stringa"
simple_title:         "Estrapolare una data da una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

In informatica, il parsing di una data da una stringa si riferisce alla conversione di una data in una forma leggibile per il computer. I programmatori spesso eseguono questa operazione quando hanno bisogno di manipolare o analizzare le date all'interno di un programma.

## Come Fare: 

Ecco un esempio di codice in Elixir che utilizza la funzione `Date.from_iso8601/1` per parsare una data dalla stringa "2020-05-01" e stamparla nel formato "1 Maggio, 2020".

```Elixir
data = "2020-05-01"
parsed_date = Date.from_iso8601(data)
IO.puts(parsed_date |> Calendar.DateTime.Format.strftime("%d %B, %Y"))
```

Output: `1 Maggio, 2020`

## Approfondimento

In passato, il parsing di una data da una stringa richiedeva molta attenzione ai dettagli e poteva essere un processo complicato e propenso agli errori. Tuttavia, con l'avvento di linguaggi di programmazione moderni come Elixir, ci sono librerie e funzioni integrate che semplificano notevolmente il processo.

Un'alternativa al parsing di una data da una stringa è quella di utilizzare librerie esterne come `Timex`, che fornisce funzionalità più avanzate e maggiori opzioni di formattazione.

Per quanto riguarda l'implementazione, la funzione `Date.from_iso8601/1` utilizza uno standard ben definito chiamato ISO 8601 per interpretare la stringa della data e restituire un oggetto di tipo `Date`.

## Vedi Anche

- [Documentazione Elixir sulla funzione `Date.from_iso8601/1`](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
- [Documentazione Timex per il parsing avanzato delle date in Elixir](https://hexdocs.pm/timex/Timex.Format.DateTime.html#parse/3)