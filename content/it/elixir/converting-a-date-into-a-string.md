---
title:                "Elixir: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché?

Esistono molte situazioni in cui potrebbe essere necessario convertire una data in una stringa. Ad esempio, se stiamo sviluppando un'applicazione che richiede l'utilizzo di dati temporali, potremmo voler visualizzare la data in un formato più semplice e facile da comprendere per gli utenti. O forse dobbiamo archiviare la data in un database come una stringa piuttosto che come un tipo di dato temporale. Qualunque sia la ragione, la conversione di una data in una stringa è un'operazione comune e importante in Elixir.

## Come fare?

Per convertire una data in una stringa in Elixir, possiamo utilizzare la funzione `to_string/2` del modulo `Calendar` che ci permette di specificare il formato di output desiderato. Vediamo un esempio con la data di oggi:

```Elixir
iex> Calendar.to_string(Calendar.local_today(), "{1}/{2}/{3}")
"12/05/2021"
```

Come possiamo vedere dall'output, abbiamo utilizzato un formato personalizzato specificando `{1}/{2}/{3}` come secondo argomento della funzione `to_string/2`. Questo ci ha dato in output la data nel formato giorno/mese/anno.

Inoltre, possiamo anche utilizzare le funzioni `strftime/2` e `strptime/2` per formattare e creare una data a partire da una stringa. Ad esempio:

```Elixir
iex> {:ok, date} = Calendar.strptime("05/12/2021", "{2}/{1}/{3}")
{:ok, ~D[2021-05-12]}

iex> Calendar.strftime(date, "%A, %B %d, %Y")
"Wednesday, May 12, 2021"
```

## Approfondimenti

Oltre alle funzioni menzionate sopra, esistono anche altre opzioni per la conversione di date in stringhe in Elixir. Possiamo utilizzare il modulo `Timex` che fornisce una vasta gamma di funzioni per la manipolazione e la formattazione delle date, o possiamo sfruttare la libreria `Ecto` per gestire la persistenza dei dati temporali nel nostro database.

Inoltre, è importante tenere in considerazione il fuso orario quando si lavora con le date in Elixir. Utilizzare il tipo di dato `DateTime` invece del tipo `Date` può essere utile per gestire i fusi orari, poiché `DateTime` tiene traccia anche dell'ora e dei minuti.

## Vedi anche

- [Elixir date/time types](https://elixir-lang.org/getting-started/datetime.html)
- [Elixir Calendar module](https://hexdocs.pm/elixir/Calendar.html)
- [Timex library](https://hexdocs.pm/timex/api-reference.html)
- [Ecto date/time types](https://hexdocs.pm/ecto/Ecto.Type.html#module-datetime-types)