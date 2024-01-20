---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Confrontare due date in programmazione equivale a determinare se una data è posteriore, precedente o uguale ad un'altra. Lo facciamo per gestire eventi temporali, come calcolare la durata tra due date, organizzare gli eventi in sequenza o verificare condizioni basate sulla data.

## Come fare:

Ruby incoraggia un linguaggio di programmazione semplice e intuitivo. Ecco come confrontare due date:

```Ruby
require 'date'

data1 = Date.new(2022, 3, 29)
data2 = Date.new(2022, 3, 30)

if data1 > data2
    puts "data1 è successiva a data2"
elsif data1 < data2
    puts "data1 è antecedente a data2"
else
    puts "data1 è la stessa di data2"
end
```
Questo script restituirà "data1 è antecedente a data2" come risultato.

## Approfondimenti:

La classe Date in Ruby è stata implementata per la prima volta nell'anno 2000 con Ruby 1.6. Ha subito numerosi miglioramenti nel corso del tempo.

Un'alternativa alla libreria Date standard di Ruby è la gemma 'timecop' che offre un modo più semplice per manipolare il tempo nella tua app. Inoltre, c'è anche 'chronic' che rende il parsing delle date molto più naturale e intuitivo.

La funzione di confronto tra due date in Ruby è implementata utilizzando il metodo di confronto `<=>` incluso nel modulo "Comparable". Questo metodo restituisce -1, 0 o 1 a seconda che la data a sinistra sia antecedente, uguale o successiva a quella a destra. Questo dettaglio di implementazione rende il confronto tra date in Ruby molto veloce e affidabile.

## Vedere Anche:

Per maggiori dettagli sulla classe Date di Ruby: https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html
Per informazioni sulla gemma 'timecop': https://github.com/travisjeffery/timecop
Per informazioni sulla gemma 'chronic': https://github.com/mojombo/chronic
Per maggiori dettagli sul modulo "Comparable": https://ruby-doc.org/core-2.7.1/Comparable.html