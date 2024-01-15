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

## Perché

Calcolare una data nel futuro o nel passato può essere utile in diversi casi, come ad esempio per pianificare eventi, gestire scadenze o analizzare dati storici.

## Come fare

Per calcolare una data in Ruby, è possibile utilizzare il metodo `Date#advance` o `Date#prev_day` che accettano come argomenti i relativi valori dell'anno, del mese e del giorno.

Ecco un esempio di codice per calcolare una data nel futuro o nel passato:

```Ruby
# Calcola la data di domani
d = Date.today.advance(days: 1)
puts d #=> 2021-07-09

# Calcola la data tra due mesi
d = Date.today.advance(months: 2)
puts d #=> 2021-09-08

# Calcola la data di ieri
d = Date.today.prev_day
puts d #=> 2021-07-07
```

Inoltre, è possibile specificare anche il numero di settimane o di anni da aggiungere o sottrarre dalla data corrente.

```Ruby
# Calcola la data tra due settimane
d = Date.today.advance(weeks: 2)
puts d #=> 2021-07-22

# Calcola la data tra tre anni
d = Date.today.advance(years: 3)
puts d #=> 2024-07-08
```

## Approfondimento

Il metodo `advance` utilizza il calendario gregoriano per calcolare le date nel futuro o nel passato. Ciò significa che tiene conto degli anni bisestili e adatta automaticamente il numero di giorni nei mesi corretti.

Inoltre, è possibile combinare più argomenti per ottenere una data molto precisa. Ad esempio, si può calcolare la data di due anni e tre mesi fa:

```Ruby
# Calcola la data di due anni e tre mesi fa
d = Date.today.advance(years: -2, months: -3)
puts d #=> 2019-04-08
```

## Vedi anche

- [Documentazione di Ruby sul metodo `advance`](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-advance)
- [Documentazione di Ruby sul metodo `prev_day`](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-prev_day)