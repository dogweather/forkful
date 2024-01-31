---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:38:22.924026-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"

category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Parsing di una data da una stringa è il processo di conversione di una data scritta come testo in un formato che Ruby può comprendere e utilizzare. I programmatori lo fanno perché nella vita reale le date vengono spesso scambiate e salvate come stringhe, e noi dobbiamo saperle interpretare e manipolare efficacemente.

## Come fare:
```Ruby
require 'date'

# Esempio di parsing di una data in formato ISO8601
data_stringa_iso = "2023-04-05"
data_parsed = Date.iso8601(data_stringa_iso)
puts data_parsed
# Output: 2023-04-05

# Parsing di una data con formato personalizzato
data_stringa_personalizzata = "05/04/2023"
data_parsed_custom = Date.strptime(data_stringa_personalizzata, '%d/%m/%Y')
puts data_parsed_custom
# Output: 2023-04-05
```

## Approfondimento
Nel mondo della programmazione, il parsing delle date è stato una necessità fin dalle prime fasi dello sviluppo software. Ruby mette a disposizione diverse classi e metodi per gestire date e orari; fra queste, `Date` e `DateTime` sono le più usate per parsing di date. Prima che venisse standardizzata la gemma 'date', diverse librerie offrivano funzionalità differenti, spesso incompatibili tra di loro.

Le alternative al parsing manualmente una data includono l'uso di gemme come 'Time' e 'Chronic', che possono semplificare ulteriormente alcune operazioni o gestire formati più complessi o naturali.

È importante notare che il parsing di date può fallire se la stringa in ingresso non corrisponde al formato atteso: è buona pratica gestire eventuali errori usando `rescue` in blocchi `begin..end` o controllando la validità della data prima di tentare il parse.

## Vedi Anche
- Documentazione Ruby ufficiale per `Date`: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- Documentazione Ruby ufficiale per `DateTime`: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)
- RubyGems per la gemma 'Chronic': [https://rubygems.org/gems/chronic/versions/0.10.2](https://rubygems.org/gems/chronic/versions/0.10.2)
