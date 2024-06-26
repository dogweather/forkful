---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:32.512808-07:00
description: "Hoe te: Ruby maakt het spelen met data kinderspel met behulp van de\
  \ ingebouwde `Date` klasse en de `active_support` gem voor wat extra suiker. Zo\
  \ wordt\u2026"
lastmod: '2024-03-13T22:44:51.359128-06:00'
model: gpt-4-0125-preview
summary: Ruby maakt het spelen met data kinderspel met behulp van de ingebouwde `Date`
  klasse en de `active_support` gem voor wat extra suiker.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
Ruby maakt het spelen met data kinderspel met behulp van de ingebouwde `Date` klasse en de `active_support` gem voor wat extra suiker. Zo wordt het gedaan:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# Vandaag's datum ophalen
today = Date.today
puts "Vandaag is: #{today}"

# Een datum 10 dagen in de toekomst berekenen
future_date = today + 10
puts "Over 10 dagen zal het zijn: #{future_date}"

# Een datum 30 dagen in het verleden berekenen
past_date = today - 30
puts "30 dagen geleden was het: #{past_date}"

# Meer complexe berekeningen met active_support
puts "Over 2 maanden zal het zijn: #{2.months.from_now.to_date}"
puts "100 dagen geleden was het: #{100.days.ago.to_date}"
```

Voorbeelduitvoer:

```
Vandaag is: 2023-04-07
Over 10 dagen zal het zijn: 2023-04-17
30 dagen geleden was het: 2023-03-08
Over 2 maanden zal het zijn: 2023-06-07
100 dagen geleden was het: 2022-12-28
```

## Diep Duiken
Voordat Ruby datumcalculatiefuncties in zijn standaard en aanvullende bibliotheken absorbeerde, moesten ontwikkelaars vaak handmatig data berekenen, rekening houdend met schrikkeljaren, verschillende maandlengtes en tijdzones - een behoorlijke hoofdpijn.

De standaard `Date` klasse doet al veel uit de doos. Je kunt gemakkelijk dagen toevoegen (`+`) of aftrekken (`-`). Echter, voor meer intuïtieve tijdperiode manipulaties, zoals "over 2 maanden", vertrouwen we op `active_support`, onttrokken uit Ruby on Rails. Deze gem gebruikt uitbreidingen op standaard Ruby klassen, waardoor dergelijke berekeningen mensvriendelijk worden.

Wanneer je verleden of toekomstige datums berekent, overweeg dan tijdszones als je ook tijden meerekent (`DateTime` of `Time` objecten). Ruby's `Time` klasse en `active_support` kunnen dit aan, maar vereisen iets meer opzet.

Er zijn alternatieven, zoals de gems `time-lord` en `ice_cube`, die respectievelijk meer syntactische suiker of gespecialiseerde functies (zoals terugkerende evenementen) bieden.

## Zie Ook
- Omgaan met tijdzones in Ruby: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- 'time-lord' gem voor meer mensachtige uitdrukkingen: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- 'ice_cube' gem voor het omgaan met terugkerende evenementen: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
