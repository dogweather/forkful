---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:13:55.355092-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in programmazione è l'atto di rilevare la data odierna dal sistema. Programmatori lo fanno per log, date di scadenza, funzionalità di calendario, e per registrare quando un evento avviene.

## How to:
Con Elixir, puoi ottenere la data corrente facilmente. Usiamo il modulo `Date` che fa parte della library standard.

```elixir
# Ottenere la data corrente in Elixir
data_corrente = Date.utc_today()
IO.inspect(data_corrente)
```

Output potrebbe essere:
```elixir
~D[2023-04-12]
```

## Deep Dive
Prima dell'Erlang/OTP 18, i programmatori erano limitati in gestione delle date e del tempo. Con l'introduzione di nuove funzionalità in versioni più recenti di Elixir, gestire il tempo è diventato molto più preciso e intuitivo. 

Programmare con le date è critico per quasi ogni sistema che interagisce con gli umani o registra eventi. Il modulo `Date` di Elixir fornisce funzioni per lavorare con le date senza l'ora, mentre per tempo preciso al millisecondo usiamo i moduli `NaiveDateTime` e `DateTime`.

Ci sono alternative come la libreria esterna "Timex" che offre funzioni avanzate per gestione di date e orari, ma per la maggior parte dei casi, il modulo interno `Date` è più che sufficiente.

La funzione `Date.utc_today/0` che abbiamo usato restituisce la data corrente in Coordinated Universal Time (UTC). Se hai bisogno della data nel fuso orario locale, puoi usare la funzione `DateTime.now/1` con un fuso orario appropriato.

## See Also
- Documentazione ufficiale di Elixir per il modulo `Date`: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Una panoramica sul modulo `DateTime`: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- Timex, una libreria ricca per lavorare con date e tempi in Elixir: [https://hexdocs.pm/timex/Timex.html](https://hexdocs.pm/timex/Timex.html)
