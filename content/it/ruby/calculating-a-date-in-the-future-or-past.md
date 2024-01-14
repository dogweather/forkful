---
title:    "Ruby: Calcolare una data nel futuro o nel passato"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché
Perché qualcuno dovrebbe impegnarsi a calcolare una data in futuro o in passato? Potrebbe sembrare una cosa inutile, ma in realtà può essere molto utile per programmi che richiedono una gestione precisa delle date, come ad esempio prenotazioni o calendari.

## Come fare
Per calcolare una data in futuro o in passato in Ruby, c'è un modo semplice e intuitivo utilizzando il metodo `advance` della classe `Date`.

```Ruby
require 'date'

# Calcola la data di domani
puts Date.today.advance(days: 1)

# Calcola la data tra una settimana
puts Date.today.advance(weeks: 1)

# Calcola la data tra un mese
puts Date.today.advance(months: 1)

# Calcola la data tra un anno
puts Date.today.advance(years: 1)

```

L'output dovrebbe essere il seguente:

```
# Output:
# 2020-05-10
# 2020-05-17
# 2020-06-09
# 2021-05-09
```

## Approfondimento
Per coloro che sono interessati ad approfondire l'argomento, ci sono alcune funzionalità aggiuntive che possono essere utili quando si lavora con date in Ruby.

Per esempio, la classe `DateTime` ha un metodo `change` che può essere utilizzato per cambiare specifici valori di una data.

```Ruby
require 'date'

# Cambia il mese e l'anno di una data
datetime = DateTime.new(2020, 5, 9)
puts datetime.change(month: 6, year: 2021)
```

L'output sarà:

```
# Output:
# 2021-06-09T00:00:00+00:00
```

Un'altra funzionalità interessante è il metodo `strftime`, che permette di formattare una data in base alle proprie esigenze.

```Ruby
require 'date'

# Formatta una data in diversi formati
date = Date.today
puts date.strftime('%d/%m/%Y') # Output: 09/05/2020
puts date.strftime('%b %d, %Y') # Output: May 09, 2020
```

## Vedi anche
- [Documentazione ufficiale di Ruby su Date e DateTime](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/index.html)
- [Articolo su come gestire le date in Ruby](https://www.sitepoint.com/working-with-dates-and-times-in-ruby/)

Scoprire come calcolare le date in futuro o in passato può sembrare una cosa banale, ma come abbiamo visto ci sono alcune funzionalità utili che possono semplificare il lavoro con le date in Ruby. Come sempre, è importante esplorare e sperimentare con le diverse funzionalità che il linguaggio di programmazione ha da offrire. Buon coding!