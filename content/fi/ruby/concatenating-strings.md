---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Ruby: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Jonojen yhdistäminen on perustoiminto, jota Ruby-ohjelmoijat käyttävät usein. Se tarkoittaa, että kaksi tai useampi merkkijonoa yhdistetään yhdeksi isoksi merkkijonoksi. Tämä on hyödyllistä esimerkiksi tekstipohjaisten sovellusten kehittämisessä, joissa halutaan luoda pidempiä lauseita tai viestejä yhdistelemällä useita lyhyitä merkkijonoja.

# Miten tehdä se?

Jonojen yhdistäminen onnistuu yksinkertaisesti käyttämällä plus-merkkiä (+) tai konkatenointioperaattoria (<<) kahden tai useamman merkkijonon välissä. Seuraavassa esimerkissä yhdistetään kolme erillistä merkkijonoa, "Tämä", "on" ja "hyvä!" yhdeksi kokonaiseksi lauseeksi "Tämä on hyvä!":

```Ruby
jono1 = "Tämä"
jono2 = "on"
jono3 = "hyvä!"

yhdessa = jono1 + " " + jono2 + " " + jono3
# Tämä on hyvä!

# tai käyttäen konkatenointioperaattoria:

yhdessa = jono1 << " " << jono2 << " " << jono3
# Tämä on hyvä!
```

# Syvä sukellus

Jonojen yhdistämisellä on tärkeä rooli ohjelmoinnissa, sillä se mahdollistaa monipuolisen ja dynaamisen tekstin luomisen. Sitä käytetään muun muassa tekstin muotoiluun ja muuttuja-arvojen yhdistämiseen osaksi isompaa merkkijonoa.

Kuitenkin kannattaa huomata, että jonojen yhdistäminen voi olla tehottomampaa kuin yhdistettyjen merkkijonojen tallentaminen uuteen muuttujaan. Myös Ruby tarjoaa muita tapoja yhdistää merkkijonoja, kuten käyttämällä interpolointia tai tekemällä merkkijonoista taulukoita.

Jonojen yhdistämisen tekniikka voi vaihdella kielestä ja ohjelmointistandardista riippuen, mutta pääperiaate pysyy yleensä samana.

# Katso myös

Jos haluat lisätietoa jonojen yhdistämisestä tai Ruby-ohjelmoinnista yleisesti, suosittelemme tutustumaan seuraaviin lähteisiin:

- Rubyn virallinen dokumentaatio (https://www.ruby-lang.org/en/documentation/)
- Codecademyn Ruby-kurssi (https://www.codecademy.com/learn/learn-ruby)
- RubyGuidesin artikkeli jonojen yhdistämisestä (https://www.rubyguides.com/2019/02/ruby-string-append-concat-operators/)