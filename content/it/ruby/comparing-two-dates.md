---
title:                "Ruby: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

In linguaggio Ruby, le date sono rappresentate come oggetti Date o Time, che possono essere facilmente comparati tra loro. Ma perché dovresti voler confrontare due date? Ci sono diverse situazioni in cui questo può essere utile in un programma. Ad esempio, potresti voler verificare se una data è precedente o successiva ad un'altra, o se si trovano entrambe in un determinato intervallo di tempo.

## Come fare

Per confrontare due date in Ruby, puoi utilizzare il metodo `.compare` che accetta due oggetti Date o Time e restituisce un valore intero. Se le due date sono identiche, il valore sarà 0. Se la prima data è precedente alla seconda, il valore sarà -1. Se la prima data è successiva alla seconda, il valore sarà 1.

```Ruby
date1 = Date.new(2021, 04, 10)
date2 = Date.new(2021, 04, 15)

# confronto tra le date usando il metodo .compare
puts "La prima data è successiva alla seconda" if date1.compare(date2) == 1
```

L'output di questo esempio sarà: La prima data è successiva alla seconda.

## Approfondimento

Esistono anche altri metodi che possono essere utili per confrontare due date in Ruby. Ad esempio, il metodo `.between?` può essere usato per verificare se una data si trova tra due date specifiche. 

```Ruby
date1 = Date.new(2021, 04, 10)
date2 = Date.new(2021, 04, 15)

# verifica se la data 12 aprile 2021 si trova tra le due date
puts "La data è compresa tra le due date" if Date.new(2021, 04, 12).between?(date1, date2)
```

L'output sarà: La data è compresa tra le due date.

Un altro metodo utile è `.same_day?` che restituisce true se due date corrispondono allo stesso giorno, mese e anno.

```Ruby
date1 = Date.new(2021, 04, 15)
date2 = Date.new(2021, 04, 15)

# verifica se le due date sono lo stesso giorno
puts "Le due date sono lo stesso giorno" if date1.same_day?(date2)
```

L'output sarà: Le due date sono lo stesso giorno.

## Vedi anche

* [Documentazione Ruby sui metodi di confronto delle date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-i-compare)
* [Tutorial su come lavorare con le date in Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)