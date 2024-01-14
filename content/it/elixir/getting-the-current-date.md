---
title:                "Elixir: Ottenere la data corrente"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un elemento fondamentale per molte applicazioni, sia per motivi di registrazione delle attività, sia per calcoli temporali. Inoltre, avere una conoscenza precisa della data e dell'ora può fornire informazioni utili per la risoluzione di problemi.

## Come fare

Per ottenere la data corrente in Elixir, possiamo utilizzare la funzione `DateTime.utc_now/0`. Dopo averla richiamata, possiamo formattare l'output secondo le nostre preferenze utilizzando `DateTime.to_string/1` e fornendo un formato come parametro.

```
Elixir
current_date = DateTime.utc_now()
formatted_date = DateTime.to_string(current_date, "{YYYY}-{MM}-{DD}")
IO.puts formatted_date

# Output: 2020-08-28
```

In alternativa, possiamo utilizzare la libreria `Calendar` per ottenere la data corrente in altre forme, come il numero del giorno dell'anno o il giorno della settimana.

```
Elixir
Calendar.local_time() |> Calendar.universal_time()

# Output: {{2020, 8, 28}, {13, 15, 43}}
```

## Approfondimento

La funzione `DateTime.utc_now/0` utilizza il sistema di data e ora UTC (Coordinated Universal Time) ed è quindi di solito preferibile rispetto alla funzione `DateTime.local_now/0`, che dipende dalle impostazioni del fuso orario del sistema in cui viene eseguita l'applicazione.

Inoltre, possiamo impostare il fuso orario desiderato utilizzando la funzione `DateTime.from_iso8601/1` e specificando il fuso orario come stringa nel formato ISO 8601: `YYYY-MM-DDThh:mm:ssTZD`.

## Vedi anche

- [Funzione `DateTime.utc_now/0` nella documentazione di Elixir](https://hexdocs.pm/elixir/DateTime.html#utc_now/0)
- [Libreria `Calendar` nella documentazione di Elixir](https://hexdocs.pm/elixir/Calendar.html)
- [Formato ISO 8601 per i fusi orari](https://en.wikipedia.org/wiki/ISO_8601#Time_zone_designators)