---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.777200-07:00
description: "Kuinka: Rubyn standardikirjasto sis\xE4lt\xE4\xE4 `Date`- ja `Time`-luokat\
  \ p\xE4iv\xE4m\xE4\xE4rien ja ajan k\xE4sittelyyn. N\xE4in saat nykyisen p\xE4iv\xE4\
  m\xE4\xE4r\xE4n."
lastmod: '2024-03-13T22:44:57.097697-06:00'
model: gpt-4-0125-preview
summary: "Rubyn standardikirjasto sis\xE4lt\xE4\xE4 `Date`- ja `Time`-luokat p\xE4\
  iv\xE4m\xE4\xE4rien ja ajan k\xE4sittelyyn."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
Rubyn standardikirjasto sisältää `Date`- ja `Time`-luokat päivämäärien ja ajan käsittelyyn. Näin saat nykyisen päivämäärän:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Esimerkkikuloste: 
```
2023-04-12
```

Jos haluat sisällyttää ajan päivämäärän lisäksi, Rubyn `Time`-luokka on sopivampi:

```ruby
current_time = Time.now
puts current_time
```

Esimerkkikuloste: 
```
2023-04-12 14:33:07 +0200
```

Jos tarvitset lisätoiminnallisuutta, kuten aikavyöhykkeen hallintaa, saatat haluta käyttää kolmannen osapuolen gemiä, kuten `ActiveSupport` (osa Railseista, mutta voidaan käyttää itsenäisesti).

Lisää ensin `activesupport` Gemfileesi ja suorita `bundle install`:

```ruby
gem 'activesupport'
```

Käytä sitten sitä aikavyöhykkeiden käsittelyyn:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Aseta haluamasi aikavyöhyke
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Esimerkkikuloste:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
