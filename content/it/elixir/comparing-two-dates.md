---
title:    "Elixir: Confrontare due date"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

Perché: Ci sono molte ragioni per cui potresti voler confrontare due date nel tuo codice Elixir. Ad esempio, potrebbe essere necessario verificare se una data cade prima o dopo di un'altra, o calcolare la differenza tra due date per scopi di pianificazione.

Come fare: Per confrontare due date in Elixir, possiamo utilizzare la funzione `Date.compare/2`. Questa funzione prende due argomenti e restituisce uno dei seguenti valori: `:before`, `:after` o `:equal`. Vediamo un esempio:

```Elixir
today = Date.utc_today() # restituisce un oggetto Date corrispondente alla data di oggi nel fuso orario UTC.
tomorrow = Date.add(today, 1, :day) # aggiunge un giorno alla data di oggi
Date.compare(today, tomorrow) # => :before
```

In questo esempio, utilizziamo la funzione `Date.utc_today()` per ottenere la data di oggi nel fuso orario UTC. Successivamente, aggiungiamo un giorno alla data di oggi usando la funzione `Date.add/3`. Infine, utilizziamo la funzione `Date.compare/2` per confrontare le due date e il risultato è `:before` poiché la data di oggi è prima della data di domani.

Deep Dive: Quando si confrontano due date in Elixir, è importante tenere presente che devono essere dello stesso tipo (Date, NaiveDateTime o DateTime). Inoltre, le date devono essere nel formato ISO8601 per essere confrontate correttamente. Ad esempio, `2021-05-10` è una data valida, ma `10-05-2021` non lo è.

Se volete calcolare la differenza tra due date, potete utilizzare la funzione `Date.diff/2`. Questa funzione restituisce il numero di giorni compresi tra le due date. Se volete ottenere la differenza in altri formati, potete utilizzare la funzione `Date.diff/3` fornendo il formato desiderato come terzo argomento.

```Elixir
birthday = Date.from_iso8601("1990-05-10") # conveniente per il confronto
Date.diff(today, birthday) # => 11360
Date.diff(today, birthday, :hours) # => 272640
```

In questo esempio, convertiamo la data di nascita in formato ISO8601 utilizzando la funzione `Date.from_iso8601/1`. Successivamente, utilizziamo la funzione `Date.diff/3` per calcolare la differenza in giorni e in ore tra la data di oggi e la data di nascita.

Vediamo ora un esempio in cui il confronto delle date può tornare utile nel nostro codice. Supponiamo di avere una lista di eventi con le relative date di inizio e fine e vogliamo filtrare solo gli eventi che si svolgono nel mese di maggio.

```Elixir
events = [
    %{
        name: "Festa della mamma",
        start_date: Date.from_iso8601("2021-05-09"),
        end_date: Date.from_iso8601("2021-05-09")
    },
    %{
        name: "Concerto di primavera",
        start_date: Date.from_iso8601("2021-04-25"),
        end_date: Date.from_iso8601("2021-05-08")
    },
    %{
        name: "Esame di Ingegneria del Software",
        start_date: Date.from_iso8601("2021-05-25"),
        end_date: Date.from_iso8601("2021-05-26")
    }
]

events_in_may = Enum.filter(events, fn event ->
    Date.compare(event.start_date, Date.beginning_of_month(today)) != :before and
    Date.compare(event.end_date, Date.end_of_month(today)) != :after
end)
```

In questo esempio, utilizziamo la funzione `Enum.filter/2` per filtrare gli eventi e ottenere solo quelli che si svolgono nel mese di maggio. Utilizziamo la funzione `Date.beginning_of_month/1` e `Date.end_of_month/1` per ottenere rispettivamente la data di inizio e fine del mese corrente e utilizziamo la funzione `Date.compare/2` per confrontare le date