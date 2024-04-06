---
date: 2024-01-20 17:37:25.673619-07:00
description: "Hvordan: Langt tilbake i Ruby's historie har `Date` og `Time` klassene\
  \ gjort det mulig \xE5 h\xE5ndtere datoer. Metoden `strftime` er universell og lar\
  \ oss\u2026"
lastmod: '2024-04-05T21:53:42.287816-06:00'
model: gpt-4-1106-preview
summary: "Langt tilbake i Ruby's historie har `Date` og `Time` klassene gjort det\
  \ mulig \xE5 h\xE5ndtere datoer."
title: Konvertere en dato til en streng
weight: 28
---

## Hvordan:
```Ruby
require 'date'

# Dagens dato
i_dag = Date.today
# Konverterer datoen til en vanlig streng
streng_format = i_dag.to_s
puts streng_format  # => "2023-04-14"

# Tilpasset datoformat med strftime
norsk_format = i_dag.strftime('%d.%m.%Y')
puts norsk_format  # => "14.04.2023"
```

## Dypdykk
Langt tilbake i Ruby's historie har `Date` og `Time` klassene gjort det mulig å håndtere datoer. Metoden `strftime` er universell og lar oss tilpasse datostrenger, som brukes mye for lokalisering. 

Alternativer inkluderer bruk av andre gems som `TimeWithZone` i Rails, som gir flere formateringsopsjoner. Når vi implementerer omformingen til en streng, må vi vurdere tidssone og å håndtere rare datoformater.

## Se Også:
- Ruby's offisielle dokumentasjon for `strftime` metoden: [strftime](https://ruby-doc.org/core/Time.html#method-i-strftime)
- Rails API-dokumentasjon om `TimeWithZone`: [TimeWithZone](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
