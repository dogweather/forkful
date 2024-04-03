---
date: 2024-01-20 17:36:09.505308-07:00
description: 'How to: In Elixir, utilizziamo il modulo `DateTime` insieme a `strftime`
  per convertire le date in stringhe formattate.'
lastmod: '2024-03-13T22:44:43.095452-06:00'
model: gpt-4-1106-preview
summary: In Elixir, utilizziamo il modulo `DateTime` insieme a `strftime` per convertire
  le date in stringhe formattate.
title: Conversione di una data in una stringa
weight: 28
---

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
