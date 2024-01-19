---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Ottenere la data corrente in Ruby
Una guida rapida e concisa

## Che cos'è & Perché?
Ottenere la data corrente nel tuo codice significa programmazione per accedere alla data e all'ora correnti del tuo sistema. Questo è effettuato dai programmatori per tracciare gli eventi, segnare i timestamp e per funzioni di data e ora relative alle operazioni.

## Come fare:
Ecco un esempio di codice su come ottenere la data corrente in Ruby.

```Ruby
require 'date'

data_corrente = Date.today
puts data_corrente
```

L'output sarà la data corrente nel formato AAAA-MM-GG.

```Ruby
# Ex: 2023-06-01
```

## Approfondimenti
Ruby, creato nel 1995, offre una diversità di metodi per gestire date e orari. Oltre a `Date.today`, metodi come `Time.now` restituiscono l'ora corrente insieme alla data.

```Ruby
ora_corrente = Time.now
puts ora_corrente
```

Questo restituirà un output nel formato AAAA-MM-GG hh:mm:ss +timezone.

Inolti, ci sono gemme (librerie) come `ActiveSupport` che rendono più facile la manipolazione delle date.

Relativamente alla implementazione, `Date.today` e `Time.now` in Ruby accedono direttamente al clock del sistema del tuo computer per ottenere la data e l'ora correnti.

## Vedi anche
Per una comprensione più approfondita di come Ruby gestisce le date e gli orari, consulta queste fonti:

- Documentation: [https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- StackOverflow: [https://stackoverflow.com/questions/8512562/how-to-get-current-date-time-in-ruby](https://stackoverflow.com/questions/8512562/how-to-get-current-date-time-in-ruby)
- Ruby Inside Custom Date and Time Formats: [https://www.rubyinside.com/custom-rails-date-and-time-formats-2874.html](https://www.rubyinside.com/custom-rails-date-and-time-formats-2874.html)