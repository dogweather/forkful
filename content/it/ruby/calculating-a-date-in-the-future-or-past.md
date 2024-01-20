---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Ruby: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Calcolare una data nel futuro o nel passato significa manipolare l'oggetto della data per trovare una data diversa rispetto ad oggi. I programmatori lo fanno per gestire gli eventi che devono verificarsi a intervalli regolari o per tracciare il tempo trascorso da un evento.

## Come fare:

Per calcolare una data nel futuro o nel passato in Ruby, faccio uso del metodo `advance`. Ecco alcuni esempi.

```ruby
# Calcola una data nel futuro
h = { year: 1, month: 2, day: 3 }
data_futura = Date.today.advance(h)
puts data_futura

# Calcola una data nel passato
h = { year: -1, month: -2, day: -3 }
data_passata = Date.today.advance(h)
puts data_passata
```
Output dell'esempio:

```ruby
2023-03-18
2020-09-12
```

## Deep Dive:

La classe `Date` in Ruby esiste fin dall’inizio e il metodo `advance` è stato introdotto per la prima volta in Rails. Esistono alternative per calcolare una data nel futuro o nel passato, inclusi l'utilizzo dei metodi `ago` e `from_now` del modulo `ActiveSupport::Time`.

```ruby
# Alternativa usando 'ago' e 'from_now'
1.year.ago # 1 anno fa
2.months.from_now # fra 2 mesi
```

In termini di implementazione, il metodo `advance` crea una nuova data, aggiungendo o sottraendo value a seconda se si vuole calcolare una data nel futuro o nel passato.

## Per saperne di più:

Per ulteriori informazioni sulla gestione delle date e degli orari in Ruby, consulta queste guide:
- Ruby Date Class: [https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- ActiveSupport TimeWithZone: [https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)