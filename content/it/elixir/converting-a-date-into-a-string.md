---
title:                "Convertire una data in una stringa"
html_title:           "Elixir: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La conversione di una data in una stringa è una delle operazioni fondamentali per ogni programmatore che lavora con date. Questo processo consiste nel trasformare un valore numerico rappresentante una data in una stringa leggibile dall'utente. I programmatori lo fanno principalmente per esporre le date in un formato comprensibile a chi utilizza il loro software.

## Come fare:
Nell'Elixir, puoi convertire una data in una stringa utilizzando la funzione `NaiveDateTime.to_string/2`. Di seguito un esempio di codice che prende una data e la converte in una stringa nel formato "aaaa-mm-gg":

```elixir
date = ~N[2022-12-31]
# => #NaiveDateTime<2022-12-31 00:00:00>
date_string = NaiveDateTime.to_string(date, "~Y-~m-~d")
# => "2022-12-31"
```

## Approfondimento:
La conversione di una data in una stringa è uno dei processi più comuni e utili nella programmazione. Oltre all'Elixir, esistono anche altri linguaggi che offrono funzioni per effettuare questa operazione, come ad esempio Python con il metodo `strftime()` o JavaScript utilizzando la libreria Moment.js. Per quanto riguarda l'implementazione, la conversione di una data in una stringa viene solitamente gestita attraverso una libreria standard di un linguaggio di programmazione, che offre funzioni utili per la formattazione delle date.

## Vedi anche:
- La documentazione ufficiale di Elixir sulla funzione `NaiveDateTime.to_string/2`: https://hexdocs.pm/elixir/NaiveDateTime.html#to_string/2
- Una guida su come formattare le date in diversi linguaggi di programmazione: https://www.howtogeek.com/50278/4-ways-to-convert-unix-timestamp-to-readable-date-in-linux/