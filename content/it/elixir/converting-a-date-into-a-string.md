---
title:                "Conversione di una data in una stringa"
date:                  2024-01-20T17:36:09.505308-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una data in una stringa"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una data in una stringa significa trasformare un oggetto che rappresenta una data e/ora in una sequenza di caratteri leggibile da un umano. I programmatori lo fanno per mostrare date in formati specifici o per inserirle in file e database che accettano solo testo.

## How to:
In Elixir, utilizziamo il modulo `DateTime` insieme a `strftime` per convertire le date in stringhe formattate.

```elixir
date = ~N[2023-04-05 13:00:00] # Creazione di un NaiveDateTime
formatted_date = NaiveDateTime.to_string(date)
IO.puts(formatted_date) # 2023-04-05 13:00:00
```

Per formati personalizzati, Elixir 1.11 ha introdotto `Calendar.strftime/3`.

```elixir
formatted_string = NaiveDateTime.strftime(date, "%d/%m/%Y %H:%M")
IO.puts(formatted_string) # 05/04/2023 13:00
```

## Deep Dive:
Convertire date in stringhe è un'esigenza comune in programmazione sin dagli albori dei computer. Per una maggiore flessibilità, Elixir ha adottato `strftime`, un sistema di formattazione che risale al linguaggio C.

Ci sono alternative. Librerie come Timex offrono funzioni aggiuntive per la gestione delle date. Tuttavia, per operazioni base, il modulo `DateTime` di Elixir è più che sufficiente.

Nei dettagli, `Calendar.strftime/3` permette di definire formati personalizzati. Il modulo `DateTime` supporta anche operazioni come la conversione da e verso epoch, la gestione dei fusi orari, e la creazione di date a partire da stringhe (parsing).

## See Also:
- Documentazione ufficiale di `DateTime` in Elixir: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- Una guida completa su `strftime` in Elixir: [https://hexdocs.pm/elixir/Calendar.html#strftime/3](https://hexdocs.pm/elixir/Calendar.html#strftime/3)
- Timex, una libreria ricca per gestire il tempo in Elixir: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)
