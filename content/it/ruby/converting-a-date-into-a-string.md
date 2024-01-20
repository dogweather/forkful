---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che Cose? E Perche?

La conversione di una data in una stringa in Ruby è un processo nel quale un oggetto `Date` viene trasformato in un formato testuale. Gli sviluppatori fanno questo per una comunicazione più semplice e per rappresentare meglio le date in un layout dell'interfaccia utente o in un output.

## Come Fai:

Convertire una data in una stringa in Ruby è abbastanza semplice grazie al metodo `strftime`. Ecco come si fa:

```Ruby
data = Date.new(2021, 8, 2)   # crea un oggetto data
stringa = data.strftime("%d/%m/%Y")  # converti la data in una stringa

puts stringa    # stampa la stringa
# Output: "02/08/2021"
```

In questo esempio, `%d`, `%m`, e `%Y` rappresentano giorno, mese, e anno, rispettivamente.

## Approfondimento:

Ruby ha introdotto il metodo `strftime` ispirandosi alla funzione omonima nel linguaggio di programmazione C. Come alternativa, Ruby offre il metodo `to_s`, che rappresenta una data in formato stringa, ma in un formato fisso (ISO 8601).

Il metodo `strftime` accetta una serie di comandi specifici per dettagliare il formato della stringa di output. Ad esempio, `%B` rappresenta il nome completo del mese mentre `%b` lo rappresenta in forma abbreviata.

## Vedi Anche:

Per approfondire la conversione di date in stringhe in Ruby, visita i link seguenti:

1. Documentazione Ruby per il metodo `strftime`: https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html#method-i-strftime
2. Guida della community Ruby sulle date: https://www.rubyguides.com/ruby-tutorial/ruby-date-format/
3. Documentazione Ruby per il metodo `to_s`: https://ruby-doc.org/core-2.7.3/Time.html#method-i-to_s