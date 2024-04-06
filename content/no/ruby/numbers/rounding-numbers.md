---
date: 2024-01-26 03:46:37.321292-07:00
description: "Hvordan: Avrunding av tall er ikke noe nytt\u2014mennesker har gjort\
  \ det i \xE5rhundrer for \xE5 gj\xF8re beregninger enklere eller for \xE5 jobbe\
  \ innenfor begrensningene\u2026"
lastmod: '2024-04-05T22:50:55.326023-06:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall er ikke noe nytt\u2014mennesker har gjort det i \xE5rhundrer\
  \ for \xE5 gj\xF8re beregninger enklere eller for \xE5 jobbe innenfor begrensningene\
  \ av deres verkt\xF8y."
title: Avrunding av tall
weight: 13
---

## Hvordan:
```Ruby
# Grunnleggende avrunding
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Spesifisere presisjon
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Avrunding ned
puts 2.9.floor          # => 2

# Avrunding opp
puts 2.1.ceil           # => 3

# Avrunding mot null
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Eksempel på utdata:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Dypere dykk
Avrunding av tall er ikke noe nytt—mennesker har gjort det i århundrer for å gjøre beregninger enklere eller for å jobbe innenfor begrensningene av deres verktøy. I Ruby er `round`-metoden allsidig, med muligheten til å avrunde til det nærmeste hele tallet som standard, eller til et spesifisert desimalplass.

Et alternativ til `round` er `floor` for alltid å runde ned, og `ceil` for alltid å runde opp, uavhengig av tallverdien. For bare å kutte av desimalplassene, har du `truncate`.

Historisk sett, når det kommer til datamaskiner, blir avrunding kritisk i håndtering av flyttallaritmetikk på grunn av dens iboende unøyaktighet. Ruby, som de fleste språk, følger IEEE 754-standarden for flyttall, noe som betyr at det håndterer avrunding på en måte som de fleste programmerere bør kunne forutsi og stole på.

Det er mer ved det, though—ting som bankmannens avrunding (også kjent som å runde halvt til jevnt) er konsepter som Ruby-utviklere kanskje må implementere manuelt, siden `round`-metoden ikke tilbyr det rett ut av boksen.

## Se også
- [Ruby-dokumentasjonen](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) for Floats' `round`-metode.
- [IEEE-standard for flyttallaritmetikk (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Forstå flyttallpresisjon](https://floating-point-gui.de/), for dypere innsikt i hvordan datamaskiner håndterer desimaltall.
