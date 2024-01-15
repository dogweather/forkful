---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Ruby: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa ohjelmoija tarvitsee tarkistaa, onko tietokoneen tiedostojärjestelmästä löytyy tietty hakemisto. Se voi olla tarpeen esimerkiksi tarkistettaessa, löytyykö tiedostoja ennen niiden käsittelyä tai luotaessa uusia tiedostoja.

## Kuinka tehdä

Ruby tarjoaa useita tapoja tarkistaa, löytyykö hakemisto tietokoneelta. Tässä esimerkkejä kahdesta erilaisesta tavasta, joilla voit toteuttaa tämän.

```ruby
# Käytä Dir.exist?() -metodia
Dir.exist?("polku/hakemistoon") #=> true tai false

# Käytä File.directory?() -metodia
File.directory?("polku/hakemistoon") #=> true tai false
```

Edellä mainitut esimerkit palauttavat joko totuusarvon "true", jos hakemisto löytyy, tai "false", jos hakemistoa ei löydy. Voit myös vaihtaa hakemiston polun mihin tahansa haluamaasi polkuun ja koodi toimii samalla tavalla.

## Syvempi sukellus

Kun tarkistat hakemiston olemassaoloa, on tärkeää huomata, että et välttämättä tarvitse tarkistaa koko tiedostojärjestelmää. Voit antaa parametrina tietyssä hakemistossa olevan polun ja Ruby tarkistaa vain tuon hakemiston.

Voit myös käyttää "Dir.glob()" -metodia, joka palauttaa taulukon tiedostoista ja hakemistoista, jotka vastaavat parametrina annettua hakemistoa.

```ruby
# Tarkista tiedostojen ja hakemistojen lukumäärä valitussa hakemistossa
Dir.glob("polku/hakemistoon/**/*").length #=> lukumäärä

# Hae tiettyä tiedostopäätettä käyttävät tiedostot
Dir.glob("polku/hakemistoon/*.rb")
```

## Katso myös

- [Ruby'n virallinen dokumentaatio hakemistojen tarkistamisesta](https://ruby-doc.org/core-2.7.2/File.html#method-c-directory-3F)
- [Ruby'n virallinen dokumentaatio hakemistojen selaamisesta](https://ruby-doc.org/core-2.7.2/Dir.html)
- [Käytännön esimerkki: Ruby-ohjelma, joka tarkistaa hakemiston olemassaolon](https://www.codingame.com/playgrounds/35462/ruby-ohjelmointikieli-harjoittelua/tehtava-7)