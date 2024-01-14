---
title:                "Elixir: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

La programmazione è un linguaggio potente e flessibile che offre molte funzionalità utili, tra cui la possibilità di calcolare le date nel futuro o nel passato. Non solo questo può sembrare molto utile, ma può anche aiutare a scrivere codice più efficiente e organizzato. In questo articolo, vedremo come utilizzare questa funzionalità in Elixir.

## Come Fare

Per calcolare una data nel futuro o nel passato, iniziamo importando il modulo `Date` di Elixir:

```Elixir
import Date # Importa il modulo Date di Elixir
```

Ora possiamo utilizzare il metodo `add` per aggiungere una certa quantità di giorni, settimane, mesi o anni a una data esistente:

```Elixir
Date.add(~D[2020-01-01], 7, :days) # Aggiunge 7 giorni alla data del 1 gennaio 2020
# Output: ~D[2020-01-08]
```

Inoltre, possiamo utilizzare il metodo `subtract` per sottrarre giorni, settimane, mesi o anni da una data esistente:

```Elixir
Date.subtract(~D[2020-01-01], 1, :months) # Sottrae un mese dalla data del 1 gennaio 2020
# Output: ~D[2019-12-01]
```

Possiamo anche utilizzare il metodo `between` per calcolare il numero di giorni o dieri settimane tra due date:

```Elixir
Date.between(~D[2020-01-01], ~D[2020-01-15]) # Calcola il numero di giorni tra il 1 e il 15 gennaio 2020
# Output: 14
```

## Approfondimento

Inoltre, il modulo `Date`di Elixir offre anche altri metodi utili per lavorare con le date. Ad esempio, possiamo utilizzare il metodo `today` per ottenere la data odierna o il metodo `days_since_epoch` per ottenere il numero di giorni trascorsi dall'epoca (1 gennaio 1970).

```Elixir
Date.today # Restituisce la data odierna
# Output: ~D[2020-07-22]

Date.days_since_epoch # Restituisce il numero di giorni trascorsi dall'epoca
# Output: 18281
```

Inoltre, il modulo `Date` supporta anche le operazioni matematiche tra due date, ad esempio la differenza tra due date o la formattazione delle date in un formato specifico.

## Vedi anche

- [Documentazione Elixir sul modulo Date](https://hexdocs.pm/elixir/Date.html)
- [Elixir School: Lavorare con le Date](https://elixirschool.com/it/lessons/specifics/dates/)
- [Elixir Date e Time](https://www.learningelixir.com/post/date-time/)