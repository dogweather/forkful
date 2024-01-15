---
title:                "Alimerkkijonojen erottaminen"
html_title:           "Ruby: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluat nimenomaan osa-merkkijonon poistamisen

Substringien poistaminen on erittäin hyödyllistä, kun haluat käsitellä vain osaa merkkijonosta tietyissä tilanteissa. Tämä voi säästää aikaa ja tehdä koodistasi tehokkaampaa.

## Kuinka: Esimerkkejä koodia ja tulostusta

Mikäli haluat poistaa osan merkkijonosta, käytä Rubyssa "slice!"-metodia, joka poistaa ilmoittamasi alueen merkkijonosta ja palauttaa poistetun osan. Katso alla olevaa esimerkkiä:

```Ruby
string = "Tämä on hyvä tapa poistaa osa merkkijonosta"

string.slice!(9, 11)

puts string #=> Tämä on poistaa merkkijonosta
```

## Syvällinen tarkastelu: Lisätietoa osa-merkkijonon poistamisesta

Jos haluat tarkistaa, onko merkkijono tietyssä kohdassa tietyllä pituudella, voit käyttää "include?"-metodia yhdessä "slice!"-metodin kanssa. Tämä auttaa sinua välttämään tyhjien alkioiden poiston, jos kohdassa ei ole tarpeeksi merkkejä poistettavaksi. Katso alla olevaa esimerkkiä:

```Ruby
string = "Tämä on toinen hyvä tapa poistaa merkkijonosta"

if string.include?("hyvä tapa") 
  string.slice!(0, 15)
end

puts string #=> poistaa merkkijonosta
```

## Katso myös

- [Ruby String API](https://ruby-doc.org/core-2.7.3/String.html)
- [Miten poistaa merkkijonosta välimerkki käyttämällä Rubya](https://medium.com/@alanachavez616/how-to-remove-punctuation-in-ruby-6d979172c5d0)
- [Stringien manipulointi ja substringien poistaminen Rubyssa](https://www.rubyguides.com/2016/07/ruby-strings/)