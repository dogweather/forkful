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

## Perché

Ogni giorno, quando interagisci con programmi e applicazioni, è probabile che tu trovi la necessità di utilizzare la data corrente. Questo può essere utile per visualizzare quando un determinato evento è avvenuto, per registrare dati o semplicemente per fornire un contesto temporale.

## Come fare

Per ottenere la data corrente in Elixir, possiamo utilizzare la funzione `DateTime.utc_now/0` che restituisce un'istanza di `DateTime` con il fuso orario UTC. Possiamo poi utilizzare alcuni metodi per ottenere informazioni specifiche sulla data, come il metodo `DateTime.day/0` per ottenere il giorno corrente.

```Elixir
current_date = DateTime.utc_now()
# => #DateTime<2021-04-21 20:00:00Z>

current_date.day()
# => 21
```

## Approfondimento

In Elixir, la data corrente è rappresentata da un'istanza di `DateTime` che include informazioni sulla data, l'ora e il fuso orario. Possiamo utilizzare metodi come `DateTime.year/0` per ottenere l'anno corrente o `DateTime.microsecond/0` per ottenere i microsecondi della data corrente.

Inoltre, possiamo utilizzare le funzioni del modulo `DateTime` per effettuare operazioni sulle date, come aggiungere o sottrarre giorni o mesi. Ad esempio, possiamo utilizzare il metodo `DateTime.add/3` per aggiungere 3 mesi alla data corrente.

```Elixir
current_date = DateTime.utc_now()
# => #DateTime<2021-04-21 20:00:00Z>

new_date = DateTime.add(current_date, 0, 3)
# => #DateTime<2021-07-21 20:00:00Z>
```

## Vedi anche

- [Dokku Current Date & Time](https://www.howtogeek.com/297269/how-to-get-the-current-date-and-time-in-ledger-format/)
- [Elixir DateTime Module](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date and Time Functions](https://www.tutorialspoint.com/elixir-programming/elixir_date_time.htm)