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

## Perché 

Comparare due date è un'operazione molto comune nella programmazione, soprattutto quando si lavora con dati temporali. Sapere come confrontare due date permette di gestire efficacemente le informazioni e di ottenere risultati precisi.

## Come fare

Per comparare due date in Ruby, è possibile utilizzare il metodo `DateTime#<=>`. Questo metodo restituisce un valore negativo se la prima data è precedente alla seconda, 0 se sono uguali e un valore positivo se la prima data è successiva alla seconda.

```Ruby
require 'date'

first_date = DateTime.new(2020, 8, 15)
second_date = DateTime.new(2020, 7, 2)

puts first_date <=> second_date # Output: 1
```

Si può anche utilizzare il metodo `DateTime#==` per controllare se due date sono uguali.

```Ruby
require 'date'

first_date = DateTime.new(2020, 8, 15)
second_date = DateTime.new(2020, 8, 15)

puts first_date == second_date # Output: true
```

## Approfondimento

Quando si confrontano due date, è importante considerare il formato dei dati. Se si ha a che fare con date generiche, si può utilizzare il metodo `Date.parse` per convertire una stringa in un oggetto di tipo date.

```Ruby
require 'date'

workshop_date = Date.parse("27-09-2020")
current_date = Date.today

puts workshop_date <=> current_date # Output: 1
```

Inoltre, Ruby offre anche diverse librerie esterne che permettono di gestire in modo più preciso le date, come ad esempio `ActiveSupport` o `Chronic`.

## Vedi anche

- Documentazione ufficiale di Ruby sulla classe `DateTime`: https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html
- Tutorial su come gestire le date in Ruby: https://www.rubyguides.com/2015/09/ruby-date-and-time/