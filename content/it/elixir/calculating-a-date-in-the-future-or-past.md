---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "Elixir: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcolo delle Date Future e Passate in Elixir

## Che cosa e Perché?

Il calcolo di una data nel futuro o nel passato è un operazione che permette di ottenere una nuova data a partire da una data iniziale, sommando o sottraendo un certo numero di giorni. I programmatori lo fanno per gestire scadenze, appuntamenti futuri, tempi di attesa e molto altro.

## Come fare:

Elixir rende facile calcolare date future o passate con il modulo `Date`. Per fare questo, usiamo la funzione `add/2`, che prende una data e un numero di giorni da aggiungere.

```Elixir
data_iniziale = Date.new(2020, 1, 1)
# {:ok, ~D[2020-01-01]}

data_futura = Date.add(data_iniziale, 30)
# {:ok, ~D[2020-01-31]}
```

Per calcolare una data nel passato, possiamo semplicemente sottrarre i giorni.

```Elixir
data_iniziale = Date.new(2020, 1, 31)
# {:ok, ~D[2020-01-31]}

data_passata = Date.add(data_iniziale, -30)
# {:ok, ~D[2019-12-02]}
```

## Approfondimento

La manipolazione delle date è un problema antico nella programmazione. Prima dell'introduzione del modulo `Date` in Elixir, gli sviluppatori dovevano fare i conti con le peculiarità dei calendari come gli anni bisestili.

Esistono alternative al modulo `Date` come `Timex` o `Calendar`, ma per la maggior parte degli usi, `Date` è più che sufficiente.

La funzione `Date.add/2` calcola il nuovo giorno, mese e anno, tenendo conto delle varie lunghezze dei mesi e dell'anno bisestile. La funzione non verifica se la data iniziale è valida, quindi è responsabilità del programmatore assicurarsi che la data sia corretta.

## Vedi Anche

- [Documentazione modulo Date](https://hexdocs.pm/elixir/Date.html)
- [Package Timex](https://hexdocs.pm/timex/readme.html)
- [Package Calendar](https://hexdocs.pm/calendar/readme.html)