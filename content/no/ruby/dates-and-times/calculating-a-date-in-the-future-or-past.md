---
date: 2024-01-20 17:31:48.334621-07:00
description: "Hvordan gj\xF8re det: ."
lastmod: '2024-03-13T22:44:41.346597-06:00'
model: gpt-4-1106-preview
summary: .
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan gjøre det:
```Ruby
require 'date'

# Legge til dager til dagens dato
dager_fremover = 10
fremtidig_dato = Date.today + dager_fremover
puts fremtidig_dato # => YYYY-MM-DD 10 dager fra nå

# Trekke fra dager fra dagens dato
dager_tidligere = 5
fortidig_dato = Date.today - dager_tidligere
puts fortidig_dato # => YYYY-MM-DD 5 dager før nå
```

Output:
```
# => 2023-10-18 (hvis dagens dato er 2023-10-08)
# => 2023-10-03 (hvis dagens dato er 2023-10-08)
```

## Dypdykk
Dato-operasjoner har vært en kjernesak i programmeringsverdenen siden begynnelsen. Ruby introduserte `Date` og `DateTime` klassene for å håndtere datoer og tidspunkter.

Alternativer:
- `Time`: Brukes for nøyaktige tidspunkter, inkludert tidssonehåndtering.
- Gems som `ActiveSupport` fra Rails-løsrivelse, tilbyr utvidede funksjoner.

Detaljer:
`Date.today + n` legger enkelt til `n` dager til den nåværende datoen. Ruby håndterer månedsskifter og skuddår automatisk. Under panseret bruker Ruby et komplekst system for dato-håndtering. For eksempel, omregning mellom datoformater og parse-funksjoner for å lese forskjellige dato-strenger.

## Se Også
- [Ruby Date-klassen Dokumentasjon](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby Time-klassen Dokumentasjon](https://ruby-doc.org/core-3.0.0/Time.html)
- [Rails ActiveSupport](https://guides.rubyonrails.org/active_support_core_extensions.html)
