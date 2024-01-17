---
title:                "Confrontare due date"
html_title:           "Gleam: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

Cosa e perchè?

Il confronto tra due date è una pratica comune nella programmazione per verificare se una data è precedente, successiva o uguale ad un'altra data. I programmatori spesso eseguono questo tipo di confronto per gestire le date in modo efficace e per automatizzare alcune azioni in base all'ordine temporale dei dati.

Come fare:
```Gleam
let start_date = Date.new(2021, 10, 25)
let end_date = Date.new(2021, 11, 2)

if start_date > end_date do
  // La data di inizio è successiva alla data di fine
end

if start_date < end_date do
  // La data di inizio è precedente alla data di fine
end

if start_date == end_date do
  // Le due date sono uguali
end
```

Deep Dive:
Il confronto tra due date è essenziale per gestire in modo accurato le date nei programmi. In passato, questo tipo di confronto veniva effettuato confrontando le date convertite in numeri interi, ma con il progresso della tecnologia e dei linguaggi di programmazione, sono state sviluppate soluzioni più efficienti. Ad esempio, in Gleam, è possibile utilizzare gli operatori '> ', '

See Also:
Per maggiori informazioni sul confronto tra due date in Gleam, consulta la documentazione ufficiale di Gleam Date (https://gleam.run/core/date) e la sezione sul confronto in gleam-extras (https://github.com/gleam-lang/gleam-extras#date-comparison).