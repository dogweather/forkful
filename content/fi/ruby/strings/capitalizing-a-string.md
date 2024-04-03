---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Miten: Ruby tarjoaa [suoraviivaisia menetelmi\xE4 merkkijonojen k\xE4\
  sittelyyn](https://docs.ruby-lang.org/en/3.3/String.html), mukaan lukien alkukirjaimen\u2026"
lastmod: '2024-03-25T19:22:43.009464-06:00'
model: gpt-4-0125-preview
summary: "Ruby tarjoaa [suoraviivaisia menetelmi\xE4 merkkijonojen k\xE4sittelyyn](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ mukaan lukien alkukirjaimen suurentaminen."
title: Merkkijonon alkukirjaimet isoiksi
weight: 2
---

## Miten:
Ruby tarjoaa [suoraviivaisia menetelmiä merkkijonojen käsittelyyn](https://docs.ruby-lang.org/en/3.3/String.html), mukaan lukien alkukirjaimen suurentaminen:

```ruby
# Rubyn sisäänrakennettu menetelmä
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Erittäin kätevä.

Rubyn `.capitalize`-metodi on kätevä, mutta se suurentaa vain ensimmäisen kirjaimen. Jos haluat enemmän kontrollia tai haluat suurentaa jokaisen sanan alkukirjaimen merkkijonossa (tunnetaan nimellä nimikkeistömuoto), saatat haluta käyttää Rails ActiveSupport-laajennuksen `titleize`-metodia tai toteuttaa sen itse:

```ruby
# Käyttäen ActiveSupportin 'titleize'-toimintoa Railsissa
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Kotitekoinen ratkaisu
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Tämä menetelmä jakaa merkkijonon sanalistaan, suurentaa kunkin sanan alkukirjaimen ja liittää ne sitten takaisin yhteen välilyönnillä.

Henkilökohtaisesti vien tämän idean paljon pidemmälle koodissani. Kirjoitin oman [`titleize`-metodin, joka ottaa huomioon pienet sanat, kuten "a" ja "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
