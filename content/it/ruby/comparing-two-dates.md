---
title:                "Confrontare due date"
html_title:           "Ruby: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Il confronto tra due date è un'operazione comune nel mondo della programmazione, che consiste nel confrontare due date per determinare l'ordine temporale in cui si verificano. I programmatori spesso confrontano due date per gestire in modo più efficace il flusso dei dati e le operazioni temporali all'interno del loro codice.

## Come fare:
Il confronto tra due date può essere fatto in diversi modi utilizzando Ruby. Uno dei modi più semplici è utilizzando il metodo `compare` della classe `Date`. Questo metodo restituisce 0 se le due date sono uguali, -1 se la prima data è precedente alla seconda e 1 se la prima data è successiva alla seconda. Ecco un esempio di codice che utilizza il metodo `compare`:

```Ruby
require 'date'

date1 = Date.parse("2021-08-01")
date2 = Date.parse("2021-08-15")
result = date1.compare(date2)

puts result # output: -1
```

Un altro modo per confrontare due date è utilizzare l'operatore di confronto `==`. Questo operatore restituisce true se le due date sono uguali e false in caso contrario. Esempio:

```Ruby
require 'date'

date1 = Date.parse("2021-08-01")
date2 = Date.parse("2021-08-15")

puts date1 == date2 # output: false
```

## Approfondimento:
Il confronto tra date è spesso necessario per gestire operazioni temporali come il calcolo della differenza tra due date, l'ordinamento di una lista di date e la verifica delle scadenze. In passato, questo tipo di operazione poteva risultare complesso e laborioso, ma grazie alla semplicità di implementazione in linguaggi di programmazione moderni come Ruby, è diventato un compito molto più semplice per i programmatori.

Oltre ai metodi descritti sopra, ci sono anche altre opzioni per confrontare due date in Ruby, come il metodo `between?` della classe `Date`, che restituisce true se la data in questione cade tra le due date fornite. Inoltre, esistono anche librerie esterne che offrono funzionalità aggiuntive per il confronto di date, come ad esempio il gem "date_diff", che consente di calcolare la differenza tra due date in diverse unità di misura come anni, mesi e giorni.

Per quanto riguarda l'implementazione dietro al confronto di date in Ruby, il linguaggio utilizza l'oggetto `Date` per rappresentare date e mette a disposizione una serie di metodi per manipolare e confrontare tali date. In particolare, Ruby segue gli standard ISO 8601 per rappresentare le date, che consente una maggiore interoperabilità tra diversi sistemi e linguaggi di programmazione.

## Vedi anche:
- [Documentazione ufficiale di Ruby sulla classe Date](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Gem "date_diff" per il calcolo della differenza tra date](https://github.com/brgordon/date_diff)
- [ISO 8601 - Standard internazionale per la rappresentazione delle date e delle ore](https://it.wikipedia.org/wiki/ISO_8601)