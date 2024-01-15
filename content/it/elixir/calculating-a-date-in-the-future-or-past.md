---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Elixir: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare la data di una giornata futura o passata può essere utile in diversi contesti, sia in ambito personale che professionale. Ad esempio, potresti voler creare una funzione che permetta di pianificare eventi o scadenze, o semplicemente avere un'indicazione precisa di quando un certo numero di giorni o mesi passeranno.

## Come Fare

È possibile utilizzare la funzione `Date.add/2` per aggiungere un numero specifico di giorni, mesi o anni a una data esistente. Ad esempio, per calcolare la data di oggi più 30 giorni, si può scrivere il seguente codice:

```elixir
today = Date.utc_today()
Date.add(today, 30)
```

Questo restituirà la data di oggi + 30 giorni. Si può anche specificare il numero di mesi o anni da aggiungere, semplicemente cambiando il secondo argomento della funzione `add`.

Per calcolare una data passata, è possibile utilizzare la funzione `Date.add/3` specificando un numero negativo come secondo argomento:

```elixir
today = Date.utc_today()
Date.add(today, -30)
```

Questa volta otterremo la data di oggi - 30 giorni.

## Approfondimento

In Elixir, le date sono rappresentate utilizzando il modulo `Date`, che fornisce numerose funzioni di utilità per lavorare con date e orari. Ci sono anche altri moduli come `DateTime` e `NaiveDateTime`, che offrono funzionalità più avanzate per gestire fusi orari e orari precisi.

Inoltre, esiste anche il modulo `Calendar`, che è altamente utilizzato per svolgere operazioni matematiche legate al calendario, come ad esempio calcolare il numero di giorni tra due date o determinare il giorno della settimana di una data specifica.

In generale, la gestione delle date e degli orari in Elixir è molto potente e intuitiva, grazie alle numerose funzioni e moduli a disposizione. Elixir utilizza il formato ISO-8601 per rappresentare le date e gli orari, il che lo rende compatibile con altri linguaggi di programmazione.

## Vedi anche

- [Documentazione ufficiale di Elixir sul modulo Date](https://hexdocs.pm/elixir/Date.html)
- [Articolo su come lavorare con calendari in Elixir](https://pragmaticstudio.com/blog/2015/6/19/working-with-calendars)
- [Tutorial su come utilizzare i moduli Date e Time in Elixir](https://www.codementor.io/@asermax/working-with-date-and-time-in-elixir-zftzt32nn)