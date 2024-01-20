---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La conversione di una data in una stringa, in Elixir, permette di rappresentare i dati di tempo in un formato leggibile. I programmatori lo fanno per semplificare la visualizzazione e il salvataggio di date per l'utente.

## Come fare:
Nella versione corrente di Elixir, trasformare una data in stringa è un'operazione semplice grazie alla funzione `Date.to_string/1`. Ecco un esempio:

```Elixir
data_corrente = Date.utc_today()
data_corrente_stringa = Date.to_string(data_corrente)
IO.puts data_corrente_stringa
```
Questo stamperà la data del giorno in un formato come `AAAA-MM-GG`.

## Approfondimenti
Fino alla versione 1.3, Elixir non aveva un modulo incorporato per gestire le date. Ci si affidava a librerie esterne come Timex. Con l'introduzione del modulo `Date` in Elixir 1.3, è diventato più semplice manipolare le date.

Un'alternativa alla conversione di una data in una stringa con `Date.to_string/1` potrebbe essere l'uso di `NaiveDateTime.to_string/1` o `DateTime.to_string/1`, a seconda se si vogliono includere le informazioni di ora e fuso orario.

L'implementazione di `Date.to_string/1` è piuttosto semplice. Elixir si basa sulla notazione ISO 8601 per le date, quindi la conversione in stringa di una data implica semplicemente la formattazione dei suoi componenti nel formato `AAAA-MM-GG`.

## Vedi anche
- La documentazione di Elixir per `Date`: https://hexdocs.pm/elixir/Date.html
- Una guida introduttiva per lavorare con date e orari in Elixir: https://blog.drewolson.org/working-with-dates-and-times-in-elixir
- L'annuncio del modulo `Date` in Elixir 1.3: https://elixir-lang.org/blog/2016/07/14/announcing-elixir-v1-3/