---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.961492-07:00
description: "\xC5 hente gjeldende dato er en avgj\xF8rende oppgave i nesten enhver\
  \ programmeringsinnsats, fra \xE5 logge aktiviteter i en applikasjon til \xE5 generere\
  \ rapporter\u2026"
lastmod: '2024-03-13T22:44:41.343413-06:00'
model: gpt-4-0125-preview
summary: "\xC5 hente gjeldende dato er en avgj\xF8rende oppgave i nesten enhver programmeringsinnsats,\
  \ fra \xE5 logge aktiviteter i en applikasjon til \xE5 generere rapporter\u2026"
title: "F\xE5 dagens dato"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å hente gjeldende dato er en avgjørende oppgave i nesten enhver programmeringsinnsats, fra å logge aktiviteter i en applikasjon til å generere rapporter med datostempler. I Ruby kan dette enkelt oppnås ved å bruke standardbiblioteket, noe som forenkler operasjoner som involverer datoer.

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
