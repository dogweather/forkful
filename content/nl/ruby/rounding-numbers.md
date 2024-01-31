---
title:                "Afronden van getallen"
date:                  2024-01-28T22:07:09.626968-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"

category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het afronden van getallen betekent het aanpassen ervan naar het dichtstbijzijnde gehele getal of naar een gespecificeerde mate van precisie. Programmeurs ronden getallen af om te vereenvoudigen, om aan menselijke verwachtingen te voldoen, of om gegevens in specifieke formaten in te passen—denk aan financiële berekeningen, grafische weergaven, of het verminderen van de opslaggrootte.

## Hoe:

```Ruby
# Basis afronding
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Precisie specificeren
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Naar beneden afronden
puts 2.9.floor          # => 2

# Naar boven afronden
puts 2.1.ceil           # => 3

# Naar nul afronden
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Voorbeelduitvoer:
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

## Diepere Duik
Het afronden van getallen is niet nieuw—mensen doen dit al eeuwen om berekeningen eenvoudiger te maken of om binnen de beperkingen van hun gereedschappen te werken. In Ruby is de `round` methode veelzijdig, met de mogelijkheid om standaard naar het dichtstbijzijnde hele getal af te ronden of naar een gespecificeerde decimale plaats.

Een alternatief voor `round` is `floor` voor altijd naar beneden afronden, en `ceil` voor altijd naar boven afronden, ongeacht de waarde van het getal. Om gewoon de decimalen weg te snijden, heb je `truncate`.

Historisch gezien wordt afronden kritiek in de omgang met floating-point rekenkunde vanwege de inherente onnauwkeurigheid. Ruby, zoals de meeste talen, volgt de IEEE 754-standaard voor floating-point getallen, wat betekent dat het afronden op een manier afhandelt die de meeste programmeurs zouden moeten kunnen voorspellen en op vertrouwen.

Er is echter meer—dingen zoals de bankiersafronding (ook bekend als afronden naar het dichtstbijzijnde even getal) zijn concepten die Ruby-ontwikkelaars mogelijk handmatig moeten implementeren, aangezien de `round` methode dit niet standaard aanbiedt.

## Zie Ook
- De [Ruby Documentatie](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) voor de `round` methode van Floats.
- [IEEE Standaard voor Floating-Point Rekenkunde (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Begrip van Floating-Point Precisie](https://floating-point-gui.de/), voor een dieper inzicht in hoe computers decimale getallen behandelen.
