---
title:                "Analisi di una data da una stringa"
html_title:           "Ruby: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Parsing di una data da una stringa si riferisce alla capacità di un programma di estrarre una data specifica da una stringa di testo. I programmatori fanno questo per convertire informazioni inserite dagli utenti in un formato leggibile e utilizzabile dal computer.

## Come:
```Ruby
Date.strptime("09/24/2021", "%m/%d/%Y")
```
Output: 24-09-2021

```Ruby
Date.parse("5th of October, 2021")
```
Output: 05-10-2021

## Approfondimento:
La funzione di parsing delle date è essenziale nella programmazione, poiché molte attività dipendono da date e orari specifici. Fin dai primi giorni della programmazione, i programmatori hanno sviluppato modi innovativi per convertire date da stringhe di testo in formati utilizzabili.

Altre alternative includono l'utilizzo di librerie esterne, come "Chronic" o "TimeParse", che facilitano il parsing delle date in formato naturale. Inoltre, i linguaggi di programmazione più recenti, come Ruby, hanno implementato funzioni di parsing delle date incorporandole direttamente nel linguaggio.

## Guarda anche:
- [Documentazione ufficiale di Ruby per parsing delle date](https://ruby-doc.org/core-3.0.2/Date.html)
- [Esempi di parsing delle date in Ruby](https://gist.github.com/laktak/3824646) 
- [Articolo su come gestire le date in Ruby](https://www.rubyguides.com/2019/02/ruby-date/)