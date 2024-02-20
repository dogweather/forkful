---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:22.201377-07:00
description: "Ottenere la data corrente in Elixir implica l'accesso alle informazioni\
  \ di data e ora del sistema, un compito comune per log, marcature di dati o\u2026"
lastmod: 2024-02-19 22:05:02.204602
model: gpt-4-0125-preview
summary: "Ottenere la data corrente in Elixir implica l'accesso alle informazioni\
  \ di data e ora del sistema, un compito comune per log, marcature di dati o\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa e perché?
Ottenere la data corrente in Elixir implica l'accesso alle informazioni di data e ora del sistema, un compito comune per log, marcature di dati o qualsiasi funzionalità che richieda la conoscenza della data corrente. Questa operazione è essenziale per creare applicazioni sensibili al tempo e per compiti come la generazione di rapporti o timestamp in un'applicazione web.

## Come fare:
La libreria standard di Elixir, tramite il modulo `DateTime`, consente di recuperare la data e l'ora correnti. Poiché Elixir è eseguito sulla VM di Erlang (BEAM), sfrutta le funzionalità sottostanti di Erlang per le operazioni temporali.

### Utilizzando la Libreria Standard di Elixir
Elixir fornisce la funzione `DateTime.utc_now/0` per ottenere la data e l'ora correnti in UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Output dell'esempio:**
```
~U[2024-02-05 19:58:40.925931Z]
```

Per ottenere solo la data corrente, si potrebbero estrarre i componenti anno, mese e giorno:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Output dell'esempio:**
```
~D[2023-05-04]
```

### Utilizzando la Libreria Timex
Per requisiti di data-ora più complessi, è possibile utilizzare una popolare libreria di terze parti chiamata Timex. Prima, aggiungi `Timex` alle tue dipendenze in mix.exs:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Dopo aver installato la dipendenza (`mix deps.get`), puoi usare Timex per ottenere la data corrente:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Output dell'esempio:**
```
~D[2023-05-04]
```

Timex offre estese funzionalità per la manipolazione di date e ore, rendendola un'aggiunta potente alle tue applicazioni Elixir, specialmente quando si ha a che fare con fusi orari, formattazione e parsing di date e orari.

Comprendendo e utilizzando le capacità integrate di Elixir e la libreria Timex, puoi lavorare facilmente con date e orari nelle tue applicazioni Elixir, adattando l'esperienza alle necessità della tua applicazione con precisione e facilità.
