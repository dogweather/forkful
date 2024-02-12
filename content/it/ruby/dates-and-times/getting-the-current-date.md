---
title:                "Ottenere la data corrente"
aliases:
- /it/ruby/getting-the-current-date/
date:                  2024-02-03T19:10:50.710274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Che cosa & Perché?
Ottenere la data corrente è un compito essenziale in quasi ogni impresa di programmazione, dal registrare le attività in un'applicazione al generare report con timbri di data. In Ruby, ciò può essere facilmente realizzato usando la libreria standard, semplificando le operazioni che coinvolgono le date.

## Come fare:
La libreria standard di Ruby include le classi `Date` e `Time` per gestire date e orari. Ecco come ottenere la data corrente:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Esempio di output: 
```
2023-04-12
```

Per includere l'ora con la data, la classe `Time` di Ruby è più adatta:

```ruby
current_time = Time.now
puts current_time
```

Esempio di output: 
```
2023-04-12 14:33:07 +0200
```

Se hai bisogno di più funzionalità, come la gestione dei fusi orari, potresti voler usare una gemma di terze parti come `ActiveSupport` (parte di Rails ma può essere usata in modo autonomo).

Prima, aggiungi `activesupport` al tuo Gemfile ed esegui `bundle install`:

```ruby
gem 'activesupport'
```

Poi, usalo per gestire i fusi orari:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Imposta il tuo fuso orario desiderato
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Esempio di output:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
