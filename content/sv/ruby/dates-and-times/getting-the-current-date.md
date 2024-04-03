---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.283869-07:00
description: "Att h\xE4mta det aktuella datumet \xE4r en v\xE4sentlig uppgift i n\xE4\
  stan alla programmeringsprojekt, fr\xE5n att logga aktiviteter i en applikation\
  \ till att generera\u2026"
lastmod: '2024-03-13T22:44:38.441818-06:00'
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet \xE4r en v\xE4sentlig uppgift i n\xE4\
  stan alla programmeringsprojekt, fr\xE5n att logga aktiviteter i en applikation\
  \ till att generera rapporter med datumst\xE4mplar."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Vad & Varför?
Att hämta det aktuella datumet är en väsentlig uppgift i nästan alla programmeringsprojekt, från att logga aktiviteter i en applikation till att generera rapporter med datumstämplar. I Ruby kan detta enkelt åstadkommas genom att använda standardbiblioteket, vilket förenklar operationer som involverar datum.

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
