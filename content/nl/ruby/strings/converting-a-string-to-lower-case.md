---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:52.696086-07:00
description: "Hoe te: Historisch gezien is het converteren van hoofdletter naar kleine\
  \ letter een basis in programmeertalen om tekstuniformiteit te waarborgen. Het\u2026"
lastmod: '2024-04-05T21:53:51.333195-06:00'
model: gpt-4-0125-preview
summary: Historisch gezien is het converteren van hoofdletter naar kleine letter een
  basis in programmeertalen om tekstuniformiteit te waarborgen.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe te:
```ruby
# Met behulp van de downcase methode
my_string = "Hello World!"
puts my_string.downcase  # => "hallo wereld!"
```

```ruby
# Gebruik van downcase! voor transformatie ter plaatse
my_string = "Hello World!"
my_string.downcase!
puts my_string           # => "hallo wereld!"
```

## Diepgaande duik
Historisch gezien is het converteren van hoofdletter naar kleine letter een basis in programmeertalen om tekstuniformiteit te waarborgen. Het ondersteunt hoofdletterongevoelige vergelijkingen en zoekopdrachten, vandaar het belang.

De methoden `downcase` en `downcase!` in Ruby komen voort uit het principe van de taal om zowel niet-destructieve als destructieve methoden voor stringmanipulatie te bieden. De niet-destructieve `downcase` geeft een nieuwe string terug, terwijl de originele onaangeroerd blijft, terwijl de destructieve `downcase!` de originele string ter plaatse wijzigt, wat geheugenefficiënter kan zijn.

Er zijn alternatieven voor gevallen waarbij locatiespecifieke regels van toepassing zijn. `String#mb_chars` gecombineerd met `ActiveSupport::Multibyte::Chars#downcase` van de Rails ActiveSupport-bibliotheek kan complexere situaties aan, zoals karakters met accenten of andere diakritische tekens:
```ruby
require 'active_support/core_ext/string/multibyte'

my_string = "ÄÖÜ"
puts my_string.mb_chars.downcase  # => "äöü"
```

Wat implementatie betreft, gebruiken Ruby's `downcase` en `downcase!` intern Unicode-mapping om elk karakter van de string naar zijn kleine letter equivalent te converteren.

## Zie ook
- Ruby-documentatie voor `downcase` en `downcase!`: [Ruby Doc downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase), [Ruby Doc downcase!](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase-21)
- Voor complexe letterconversies, zie de ActiveSupport Core Extensions: [ActiveSupport String](https://api.rubyonrails.org/classes/String.html)
