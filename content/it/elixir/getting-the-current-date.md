---
title:                "Elixir: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ogni volta che scriviamo un'applicazione, spesso abbiamo bisogno di gestire date e orari. Che sia per tenere traccia degli eventi futuri, calcolare intervalli di tempo o semplicemente visualizzare la data corrente, dobbiamo avere gli strumenti giusti a portata di mano. In Elixir, ottenere la data corrente è un'operazione semplice e veloce, e in questo post ti mostrerò come.

## Come Fare

Per ottenere la data e l'orario corrente in Elixir, possiamo utilizzare la funzione di libreria `DateTime.utc_now/0` che restituisce un oggetto `DateTime` con il fuso orario impostato su UTC. Vediamo un esempio di codice:

```Elixir
current_datetime = DateTime.utc_now()
```

Questo ci restituirà un oggetto `DateTime` simile a questo: `~U[2020-09-09 19:30:45]`. Per visualizzare solo la data corrente, possiamo utilizzare il metodo `DateTime.date/1`:

```Elixir
current_date = DateTime.date(current_datetime)
```

In questo caso, la variabile `current_date` conterrà solo la data corrente, senza l'orario: `~D[2020-09-09]`. Anche se la data e l'orario sono rappresentati come stringhe, è importante notare che in realtà sono oggetti, e possono essere manipolati e formattati come tali.

### Formattazione della Data

Ora che abbiamo ottenuto la data, potremmo aver bisogno di formattarla per renderla più leggibile o adatta alle nostre esigenze. Per fare ciò, possiamo utilizzare il modulo `DateTime` che offre diverse funzioni per la formattazione delle date. Ad esempio, possiamo utilizzare la funzione `DateTime.to_iso8601/1` per convertire la data in un formato standardizzato ISO 8601:

```Elixir
current_date_iso = DateTime.to_iso8601(current_date)
```

Il valore di `current_date_iso` sarà ora `2020-09-09`, che è più leggibile rispetto al formato originale. Puoi consultare la documentazione di Elixir per maggiori dettagli sulla formattazione della data e sull'utilizzo di altre funzioni utili.

## Approfondimento

Oltre alla funzione `DateTime.utc_now/0`, ci sono altre opzioni per ottenere la data corrente in Elixir. Ad esempio, possiamo utilizzare il modulo `Date` per ottenere la data corrente locale, oppure il modulo `Time` per ottenere l'ora corrente senza la data. Inoltre, è possibile impostare il fuso orario locale utilizzando il modulo `TimeZone` e ottenere la data e l'orario in quel fuso orario specifico.

## Vedi Anche

- [Documentazione su DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Documentazione su Date](https://hexdocs.pm/elixir/Date.html)
- [Documentazione su Time](https://hexdocs.pm/elixir/Time.html)
- [Documentazione su TimeZone](https://hexdocs.pm/elixir/TimeZone.html)