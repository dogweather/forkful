---
title:    "Elixir: Ottenere la data attuale"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Sapere la data corrente può essere utile in varie situazioni di programmazione, come ad esempio nel log degli eventi o nel controllo della validità di una licenza.

## Come ottenere la data corrente in Elixir

Per ottenere la data corrente in Elixir, è possibile utilizzare la funzione `Calendar.local_time/0` che restituisce una tupla contenente il fuso orario e l'ora locale.

```Elixir
{timezone, local_time} = Calendar.local_time()
```

Se si vuole ottenere solo l'ora locale, è possibile utilizzare la funzione `Calendar.local_time/1` specificando come parametro il fuso orario desiderato.

```Elixir
local_time = Calendar.local_time("America/New_York")
```

È anche possibile utilizzare la libreria `Timex` per ottenere la data e l'ora attuali in un formato specifico. La libreria deve essere prima installata tramite il comando `mix deps.get`, poi è possibile utilizzare la funzione `Timex.utc_now/0` per ottenere la data e l'ora UTC come oggetto.

```Elixir
require Timex

urrent_time = Timex.utc_now()
```

La libreria `Timex` offre anche la possibilità di formattare la data e l'ora in vari formati, ad esempio utilizzando la funzione `Timex.format/2`. In questo esempio, verrà utilizzato il formato ISO 8601.

```Elixir
formatted_time = Timex.format({{2021, 12, 31}, {23, 59, 59}}, "{ISO:Extended:Weekday}")
```

Output:

```Elixir
"2021-12-31T23:59:59"
```

## Approfondimento

La funzione `Calendar.local_time/0` utilizza il fuso orario del sistema operativo in cui è in esecuzione il codice. Se si vuole ottenere una data specifica, è possibile utilizzare la funzione `Calendar.local_time/1` specificando il fuso orario desiderato.

Inoltre, la libreria `Timex` offre molte funzionalità aggiuntive per la gestione del tempo e degli intervalli, come la conversione tra fusi orari e la manipolazione di date e orari.

## Vedi anche

- Documentazione Elixir sulla funzione `Calendar.local_time/0`: https://hexdocs.pm/elixir/Calendar.html#local_time/0
- Documentazione Elixir sulla libreria `Timex`: https://hexdocs.pm/timex/readme.html