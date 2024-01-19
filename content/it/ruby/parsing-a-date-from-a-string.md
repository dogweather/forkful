---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analizzare una Data da una Stringa in Ruby

## Che cosa e Perché?
L'analisi di una data da una stringa è il processo per convertire un testo (stringa) in un oggetto di data. I programmatori lo fanno perché è comodo rappresentare le date come stringhe ma, per lavorare con esse, abbiamo bisogno di convertirle in oggetti di data.

## Come Fare:
In Ruby, possiamo utilizzare il modulo interno `Date` per analizzare la data. Ecco un breve esempio:
```Ruby 
require 'date'

data_stringa = "2022-07-10"
data_oggetto = Date.parse(data_stringa)

puts data_oggetto
```
Produzione:
```Ruby 
2022-07-10
```

## Approfondimento
Storicamente, l'analisi delle date era un po' più complicata in Ruby. Prima di Ruby 1.9, la conversione di una stringa in date non era supportata internamente. Ma ora, è molto semplice grazie al modulo Date.

Ci sono altre librerie, come `Delorean`, che può fare l'analisi delle date ma per la maggior parte dei casi, il modulo `Date` dovrebbe essere sufficiente.

Riguardo ai dettagli dell'attuazione, il metodo `Date.parse` funziona identificando e analizzando le parti della stringa date che sembrano essere componenti della data (come l'anno, il mese e il giorno). Se non riesce a determinare una parte deterministica della data, fa delle ipotesi.

## Vedi Anche
- [Modulo Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Delorean Gem](https://github.com/bebanjo/delorean)
- [Come analizzare le date in Ruby - Stack Overflow](https://stackoverflow.com/questions/3915072/how-to-parse-a-date-in-ruby)
- [Documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/documentation/)