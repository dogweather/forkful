---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.283869-07:00
description: "Hur man g\xF6r: Rubys standardbibliotek inkluderar klasserna `Date`\
  \ och `Time` f\xF6r att hantera datum och tid. S\xE5 h\xE4r f\xE5r du det aktuella\
  \ datumet."
lastmod: '2024-03-13T22:44:38.441818-06:00'
model: gpt-4-0125-preview
summary: "Rubys standardbibliotek inkluderar klasserna `Date` och `Time` f\xF6r att\
  \ hantera datum och tid."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur man gör:
Rubys standardbibliotek inkluderar klasserna `Date` och `Time` för att hantera datum och tid. Så här får du det aktuella datumet:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Exempelutskrift: 
```
2023-04-12
```

För att inkludera tid med datumet är Rubys `Time`-klass mer lämplig:

```ruby
current_time = Time.now
puts current_time
```

Exempelutskrift: 
```
2023-04-12 14:33:07 +0200
```

Om du behöver mer funktionalitet, såsom hantering av tidszoner, kanske du vill använda ett tredjepartsgem som `ActiveSupport` (del av Rails men kan användas fristående).

Först, lägg till `activesupport` i din Gemfile och kör `bundle install`:

```ruby
gem 'activesupport'
```

Därefter, använd det för att hantera tidszoner:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Ange din önskade tidszon
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Exempelutskrift:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
