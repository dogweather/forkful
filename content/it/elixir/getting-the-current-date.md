---
title:                "Ottenere la data corrente"
html_title:           "Elixir: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente in Elixir significa ottenere l'informazione sul giorno, il mese e l'anno attuali. I programmatori spesso utilizzano questa informazione per registrare l'orario delle operazioni o per effettuare calcoli basati sulla data.

## Come ottenere la data attuale:

```Elixir
Date.utc_today()
```

Output:

```Elixir
~D[2021-10-19]
```

Per ottenere la data attuale in un fuso orario specifico, è possibile utilizzare il seguente codice:

```Elixir
DateTime.utc_now()
|> DateTime.to_time_zone("Europe/Rome")
|> Timex.to_date()
```

Output:

```Elixir
~D[2021-10-19]
```

## Approfondimento:

Ottenere la data attuale è un'operazione comune in molte applicazioni, come ad esempio nei sistemi di prenotazione o nei calendari. In Elixir, esistono diverse librerie che possono essere utilizzate per ottenere informazioni sulla data e sull'ora corrente, come ad esempio Timex o Erlang's calendar module.

Un'alternativa all'utilizzo di librerie esterne è utilizzare la funzione built-in `:calendar.local_time()` che restituisce la data e l'ora correnti come una tupla di tre elementi: `{anno, mese, giorno}`.

Per ottenere informazioni più dettagliate sulla data e sull'ora, è possibile utilizzare la funzione `DateTime.utc_now()` che restituisce un'istante di tempo con la precisione al secondo.

## Vedi Anche:

- Documentazione ufficiale di Elixir sulla gestione delle date e delle ore: https://hexdocs.pm/elixir/DateTime.html
- Libreria Timex per la manipolazione delle date e delle ore in Elixir: https://github.com/bitwalker/timex