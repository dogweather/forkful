---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.710274-07:00
description: 'Come fare: La libreria standard di Ruby include le classi `Date` e `Time`
  per gestire date e orari. Ecco come ottenere la data corrente.'
lastmod: '2024-03-13T22:44:44.061538-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Ruby include le classi `Date` e `Time` per gestire
  date e orari.
title: Ottenere la data corrente
weight: 29
---

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
