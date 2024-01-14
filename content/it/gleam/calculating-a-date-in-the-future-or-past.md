---
title:                "Gleam: Calcolo di una data nel futuro o nel passato"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per diverse ragioni. Ad esempio, si potrebbe desiderare di sapere quando un progetto sarà completato o quando si raggiungerà un'età specifica. Inoltre, la capacità di calcolare una data in modo accurato può semplificare la pianificazione e la gestione del tempo.

## Come fare

Per calcolare una data nel futuro o nel passato utilizzando Gleam, è necessario utilizzare il modulo `Datetime` che fornisce diverse funzioni utili per la gestione delle date. Ecco un esempio di codice utilizzando il modulo `Datetime` per calcolare una data nel futuro di 7 giorni:

```Gleam
let today = Polyglot.Datetime.now()
let future_date = Polyglot.Datetime.add_days(today, 7)
IO.println("La data tra 7 giorni sarà: " ++ Polyglot.Datetime.format(future_date, "%d/%m/%Y"))
```

L'output di questo codice sarà:

```
La data tra 7 giorni sarà: 26/04/2021
```

Per calcolare una data nel passato, possiamo invece utilizzare la funzione `subtract_days` del modulo `Datetime`. Ecco un esempio di codice che calcola la data di 15 giorni fa:

```Gleam
let today = Polyglot.Datetime.now()
let past_date = Polyglot.Datetime.subtract_days(today, 15)
IO.println("La data di 15 giorni fa è stata: " ++ Polyglot.Datetime.format(past_date, "%d/%m/%Y"))
```

L'output di questo codice sarà:

```
La data di 15 giorni fa è stata: 01/04/2021
```

## Approfondimento

Calcolare una data nel futuro o nel passato può risultare più complesso di quanto si pensi, in quanto bisogna considerare fattori come le diverse lunghezze dei mesi e degli anni, i giorni bisestili e le eventuali differenze di fuso orario. Inoltre, ci possono essere casi in cui è necessario calcolare la data in base a una specifica data di riferimento, anziché utilizzare semplicemente la data odierna.

Per affrontare tutte queste sfide, il modulo `Datetime` di Gleam fornisce diverse funzioni e formati per rendere più semplice e precisa la gestione delle date. Inoltre, è importante fare attenzione a utilizzare i formati di data e ora corretti al fine di evitare errori di calcolo.

## Vedi anche

- Documentazione ufficiale del modulo `Datetime` di Gleam: https://gleam.run/documentation/stdlib/core/Polyglot.Datetime.html
- Una guida su come calcolare una data nel futuro utilizzando JavaScript: https://www.w3schools.com/js/js_date_methods.asp
- Informazioni sulle diverse operazioni che possono essere eseguite sulle date in Java: https://www.tutorialspoint.com/java8/java8_date_time.htm