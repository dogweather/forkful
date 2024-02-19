---
aliases:
- /fi/ruby/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.777200-07:00
description: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hakeminen on olennainen teht\xE4\
  v\xE4 l\xE4hes miss\xE4 tahansa ohjelmointiprojektissa, oli kyse sitten toimintojen\
  \ lokituksesta sovelluksessa\u2026"
lastmod: 2024-02-18 23:09:08.194217
model: gpt-4-0125-preview
summary: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hakeminen on olennainen teht\xE4v\xE4\
  \ l\xE4hes miss\xE4 tahansa ohjelmointiprojektissa, oli kyse sitten toimintojen\
  \ lokituksesta sovelluksessa\u2026"
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän hakeminen on olennainen tehtävä lähes missä tahansa ohjelmointiprojektissa, oli kyse sitten toimintojen lokituksesta sovelluksessa tai päiväleimoilla varustettujen raporttien tuottamisesta. Rubyssa tämä on helposti saavutettavissa käyttämällä standardikirjastoa, mikä yksinkertaistaa päivämääriä sisältäviä operaatioita.

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
