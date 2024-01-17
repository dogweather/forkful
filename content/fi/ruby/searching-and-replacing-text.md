---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Ruby: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hakeminen ja korvaaminen tarkoittavat tekstissä esiintyvien tiettyjen merkkijonojen etsimistä ja niiden korvaamista toisilla merkkijonoilla. Ohjelmoijat tekevät tätä voidakseen tehokkaasti muokata ja päivittää suuria määriä dataa, esimerkiksi kooditiedostoja.

## Näin teet sen:

```Ruby
teksti = "Tervetuloa Ruby:n ihmeelliseen maailmaan!"

# Etsi sana "maailma" ja korvaa se sanalla "kieli"
puts teksti.gsub("maailma", "kieli")

# => Tervetuloa Ruby:n ihmeelliseen kieliin!
```

## Sukella syvemmälle:

Hakeminen ja korvaaminen ovat olleen käytössä jo kauan ennen Ruby-ohjelmointikielen keksimistä. On olemassa myös muita tapoja suorittaa tämä toiminto, kuten käyttämällä säännöllisiä lausekkeita tai erikoistyökaluja, kuten sed ja awk. Rubyssa hakeminen ja korvaaminen tehdään ```gsub```-metodilla, joka toimii vastaavalla tavalla kuin sed.

## Katso myös:

- [Ruby String -dokumentaatio](https://ruby-doc.org/core-2.6.3/String.html)
- [Artikkeli hakemisesta ja korvaamisesta Rubyssa](https://www.sitepoint.com/find-replace-words-in-ruby/)
- [Perl Compatible Regular Expression](https://rubular.com/), työkalu säännöllisten lausekkeiden testaamiseen ja muokkaamiseen.