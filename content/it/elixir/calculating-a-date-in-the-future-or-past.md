---
title:    "Elixir: Calcolare una data nel futuro o nel passato."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché 

Calcolare una data nel futuro o nel passato può essere utile in diversi contesti di programmazione, come ad esempio nella creazione di reminder o nella gestione di eventi temporizzati. Elixir offre una semplice e potente sintassi per svolgere questo tipo di operazioni, rendendo il processo veloce ed efficiente.

## Come fare

In Elixir, è possibile calcolare una data nel futuro o nel passato utilizzando il modulo `Timex`. Questo modulo fornisce una serie di funzioni per lavorare con date e orari, tra cui `add`, che ci permette di aggiungere un certo numero di giorni, mesi o anni a una data specifica.

Ecco un esempio di codice che ci permette di calcolare la data di oggi più 3 giorni in futuro utilizzando il modulo `Timex`:

```
Elixir -  Code Block

import Timex

today = Timex.now()
future_date = Timex.add(today, 3, :days)

IO.puts "La data di oggi è #{today} e tra 3 giorni sarà #{future_date}."
```

Il codice sopra descritto utilizza la funzione `now` di `Timex` per ottenere la data di oggi e poi la funzione `add` per aggiungere 3 giorni alla data ottenuta. Infine, con la funzione `IO.puts`, visualizziamo la data di oggi e quella calcolata per il futuro.

Il modulo `Timex` offre anche altre funzioni utili per il calcolo di date nel futuro o nel passato, come ad esempio `subtract` per sottrarre giorni, mesi o anni e `shift` per spostare una data in avanti o indietro di una certa quantità di tempo.

## Approfondimento

In Elixir, le date vengono rappresentate utilizzando il formato ISO 8601, che consiste in una combinazione di anno, mese e giorno. Inoltre, le funzioni del modulo `Timex` sono progettate per gestire anche i fusi orari in modo efficace, garantendo la corretta gestione dei fusi orari quando si lavora con date e orari.

Oltre al modulo `Timex`, Elixir offre anche altri strumenti utili per lavorare con date e orari, come ad esempio il modulo `Calendar`, che fornisce funzioni per ottenere informazioni dettagliate su una determinata data, come il giorno della settimana o il numero della settimana.

## Vedi anche

- [Documentazione ufficiale di Elixir su Timex](https://hexdocs.pm/timex/)
- [Tutorial sull'utilizzo dei moduli di date in Elixir](https://www.codedrome.com/elixir-date-manipulation/)
- [Altri strumenti utili per lavorare con le date in Elixir](https://medium.com/@kevinlamping/beyond-timex-going-deeper-with-dates-in-elixir-195b715d8c10)