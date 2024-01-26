---
title:                "Confronto tra due date"
date:                  2024-01-20T17:32:58.667444-07:00
model:                 gpt-4-1106-preview
simple_title:         "Confronto tra due date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparare due date significa verificare se sono uguali, quale precede o segue l'altra. I programmatori lo fanno per gestire eventi, scadenze e funzionalità legate al tempo.

## How to:
```elixir
# Creazione di date
date1 = ~D[2023-03-15]
date2 = ~D[2023-04-01]

# Confronto date
is_before = Date.compare(date1, date2) == :lt
is_same = Date.compare(date1, date2) == :eq
is_after = Date.compare(date1, date2) == :gt

# Output
IO.puts("date1 è prima di date2? #{is_before}") # date1 è prima di date2? true
IO.puts("date1 è la stessa di date2? #{is_same}") # date1 è la stessa di date2? false
IO.puts("date1 è dopo di date2? #{is_after}") # date1 è dopo di date2? false
```

## Deep Dive
Elixir fornisce la `Date.compare/2` per confrontare due date. La funzione ritorna `:lt` (less than), `:eq` (equal) o `:gt` (greater than). Prima dell'1.3, confrontare le date era meno intuitivo e richiedeva più codice. Implementazioni alternative preesistenti erano spesso fatte tramite librerie esterne come Timex. Internamente, `Date.compare/2` converte le date in valori integer e poi li confronta, permettendo un confronto efficiente e preciso.

## See Also
- Documentazione ufficiale di Elixir sul modulo `Date`: https://hexdocs.pm/elixir/Date.html
- Libreria Timex, per gestione avanzata del tempo in Elixir: https://hex.pm/packages/timex
- Guide introduttive a Elixir: https://elixir-lang.org/getting-started/introduction.html
