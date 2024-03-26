---
title:                "Merkkijonon käyttäminen isolla alkukirjaimella"
date:                  2024-03-25T17:31:57.214652-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon alkukirjaimen muuttaminen isoksi tarkoittaa yleensä merkkijonon ensimmäisen merkin muuttamista isoksi kirjaimeksi ja lopun pieniksi kirjaimiksi. Joskus se voi kuitenkin tarkoittaa vain varmistamista, että ensimmäinen merkki on iso kirjain, jättäen lopun merkkijonon muuttumattomaksi. Rehellisesti sanottuna, mielestäni se on hieman epämääräinen termi.

## Kuinka:
Ruby tarjoaa [suoraviivaisia menetelmiä merkkijonon käsittelyyn](https://docs.ruby-lang.org/en/3.3/String.html), mukaan lukien alkukirjaimen suurentaminen:

```ruby
# Rubyn sisäänrakennettu metodi
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Erittäin kätevää.

Rubyn `.capitalize` metodi on kätevä, mutta se tekee isoksi vain ensimmäisen kirjaimen. Jos haluat enemmän kontrollia tai haluat muuttaa jokaisen sanan alkukirjaimen isoksi (tunnettu nimellä nimikekirjoitus), saatat haluta käyttää Rails ActiveSupport -laajennuksen `titleize` metodia tai toteuttaa sen itse:

```ruby
# Käyttäen ActiveSupportin 'titleize' metodia Railsissa
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

Tämä menetelmä jakaa merkkijonon sanojen taulukoksi, suurentaa jokaisen sanan alkukirjaimen ja yhdistää ne sitten takaisin yhteen välilyönnin kera.

Henkilökohtaisesti vien tämän idean paljon pidemmälle koodissani. Kirjoitin oman [`titleize` metodini, joka ottaa huomioon pienet sanat kuten "a" ja "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
