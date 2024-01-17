---
title:                "Convertire una data in una stringa"
html_title:           "Ruby: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Cosa & Perché?
Convertire una data in una stringa è un'operazione comune nella programmazione, in cui una data viene trasformata in una rappresentazione testuale leggibile per gli utenti. I programmatori fanno questo per facilitare la comunicazione delle informazioni riguardo al tempo e alle date ai loro utenti.

## Come fare:
```Ruby
time = Time.now
puts time.to_s # output: 2021-02-25 17:30:00 +0900
```

```Ruby
date = Date.today
puts date.to_s # output: 2021-02-25
```

## Approfondimento:
La conversione di una data in una stringa ha origini storiche, poiché le prime lingue di programmazione non avevano un tipo di dato specifico per le date. Oggi, ci sono diverse alternative per questo tipo di operazione, come l'utilizzo di librerie esterne specializzate o l'utilizzo di formati specifici come ISO 8601. L'implementazione di questa conversione dipende dal linguaggio di programmazione utilizzato e dalle esigenze specifiche del progetto.

## Vedi anche:
- https://ruby-doc.org/core-3.0.0/Time.html#method-i-to_s
- https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html#method-i-to_s