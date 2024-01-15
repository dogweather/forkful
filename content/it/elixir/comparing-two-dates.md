---
title:                "Confronto di due date"
html_title:           "Elixir: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date può sembrare un'operazione banale, ma in realtà è molto importante per garantire che i dati siano correttamente ordinati e gestiti. Inoltre, può essere utile nella logica di programmazione per verificare se una data è precedente, successiva o uguale ad un'altra.

## Come fare

Elixir offre una serie di funzioni per aiutare nella comparazione di date. Vedremo di seguito alcuni esempi di codice per comprendere meglio il processo.

```Elixir
date1 = ~D[2021-05-25] # Definiamo una data usando il formato YYYY-MM-DD
date2 = ~N[2021-05-26 08:00:00] # Definiamo una data e un orario usando il formato YYYY-MM-DD HH:mm:ss

date1 > date2 # Output: false
date1 < date2 # Output: true
date1 == date2 # Output: false

# Possiamo anche aggiungere o sottrarre giorni, mesi o anni dalle date

date1 + 5 # Output: ~D[2021-05-30] aggiunge 5 giorni
date2 - ~D[0, 0, 0, 3] # Output: ~N[2021-05-23 08:00:00] sottrae 3 mesi
```

## Approfondimento

La comparazione di date in Elixir è possibile grazie al modulo `DateTime`, che offre una vasta gamma di funzioni per manipolare le date e gli orari. É importante notare che Elixir supporta anche i fusi orari, che possono essere gestiti tramite la libreria `Tzdata`.

Per ulteriori informazioni sulla gestione delle date in Elixir, si consiglia di consultare la documentazione ufficiale: [https://hexdocs.pm/elixir/](https://hexdocs.pm/elixir/).

## Vedi anche

- [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
- [https://github.com/lau/calendar](https://github.com/lau/calendar)
- [https://github.com/lau/timex](https://github.com/lau/timex)