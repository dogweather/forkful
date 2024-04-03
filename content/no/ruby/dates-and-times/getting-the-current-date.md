---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.961492-07:00
description: "Hvordan: Rubys standardbibliotek inkluderer klassene `Date` og `Time`\
  \ for h\xE5ndtering av datoer og tid. Slik f\xE5r du tak i gjeldende dato."
lastmod: '2024-03-13T22:44:41.343413-06:00'
model: gpt-4-0125-preview
summary: "Rubys standardbibliotek inkluderer klassene `Date` og `Time` for h\xE5ndtering\
  \ av datoer og tid."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
Rubys standardbibliotek inkluderer klassene `Date` og `Time` for håndtering av datoer og tid. Slik får du tak i gjeldende dato:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Eksempel på utdata:
```
2023-04-12
```

For å inkludere tid sammen med datoen, er Rubys `Time` klasse mer passende:

```ruby
current_time = Time.now
puts current_time
```

Eksempel på utdata:
```
2023-04-12 14:33:07 +0200
```

Hvis du trenger mer funksjonalitet, som håndtering av tidssoner, kan du ønske å bruke en tredjepartsgem som `ActiveSupport` (del av Rails, men kan brukes alene).

Først, legg til `activesupport` i din Gemfile og kjør `bundle install`:

```ruby
gem 'activesupport'
```

Deretter, bruk den til å håndtere tidssoner:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Sett din ønskede tidssone
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Eksempel på utdata:
```
Ons, 12 Apr 2023 08:33:07 EDT -04:00
```
