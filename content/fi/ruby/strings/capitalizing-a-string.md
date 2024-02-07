---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-02-03T19:06:21.983477-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Merkkijonon alkukirjaimen suurentaminen ohjelmoinnissa viittaa yleensä ensimmäisen merkin muuntamiseen suuraakkoseksi ja lopun pienaakkosiksi. Ohjelmoijat tekevät näin syistä kuten noudattaakseen nimeämiskonventioita, parantaakseen tulosteiden luettavuutta tai varmistaakseen tietojen johdonmukaisuuden vertailuissa ja tallennuksessa.

## Miten:
Ruby tarjoaa suoraviivaisia ​​metodeja merkkijonojen käsittelyyn, mukaan lukien alkukirjaimen suurentaminen. Näin voit suurentaa merkkijonon alkukirjaimen Rubylla:

```ruby
# Rubyn sisäänrakennettu metodi
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Rubyn `.capitalize` metodi on kätevä, mutta se vaikuttaa vain ensimmäiseen kirjaimeen. Jos haluat enemmän hallintaa tai suurentaa jokaisen sanan alkukirjaimen merkkijonossa (tunnetaan nimellä nimiraja), saatat haluta käyttää `titleize` metodia Railsin ActiveSupport-laajennuksesta, tai toteuttaa sen itse:

```ruby
# Käyttäen ActiveSupportin 'titleize' Railsissa
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Jos et käytä Railsia tai suosit puhdasta Ruby-ratkaisua, tässä on miten saatat suurentaa jokaisen sanan alkukirjaimen merkkijonossa:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Tämä menetelmä jakaa merkkijonon sanalistan, suurentaa jokaisen sanan alkukirjaimen, ja liittää ne sitten takaisin yhteen välilyönnillä.
