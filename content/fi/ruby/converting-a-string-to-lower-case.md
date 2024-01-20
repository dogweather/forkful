---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Muuttaminen Merkkijonosta Pieneen Kirjaimistoon Ruby-ohjelmassa

## Mikä ja Miksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi on prosessi, jossa kaikki merkkijonon isot kirjaimet korvataan pienillä kirjaikilla. Ohjelmoijat tekevät tämän usein tietojen normalisointia ja vertailua varten.

## Kuinka Tehdä:

Alla on koodiesimerkki kuinka muuntaa merkkijono pieniksi kirjaimiksi Rubyssa:

```Ruby
iso_kirjain = "SUOMI"
pieni_kirjain = iso_kirjain.downcase

puts pienen_kirjain
```

Tulostuksesta tulee:

```Ruby
"suomi"
```
Tämä `downcase`-menetelmä muuntaa kaikki merkkijonon kirjaimet pieniksi kirjaimiksi.

## Syvällinen Tieto

Historiallisesti Ruby-ohjelmointikieli on tukenut merkkijonojen muuttamisen pieniksi kirjaimiksi alusta asti, joka osoittaa tämän toiminnallisuuden tärkeyden. Jotkin vaihtoehdot tämän toiminnallisuuden toteuttamiseen sisältävät oman menetelmän kirjoittamisen manuaalisesti, mutta tämä ei ole yhtä tehokasta kuin `downcase` -menetelmä Rubyssa.

Ilmeisyyden vuoksi, kulissien takana `downcase` vertaa jokaista merkkijonon kirjainta Unicode-taulukon kanssa. Jos kirjain on iso, se korvataan vastaavalla pienellä kirjaimella.

## Katso Lisää

Jos haluat tietää lisää Ruby-ohjelmoinnista, tutustu seuraaviin linkkeihin:

- [Ruby Official Documentation](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)
- [Ruby Guides - Strings in Ruby](https://www.rubyguides.com/2015/05/ruby-string-methods/) 

Tämän lisäksi, merkkijonotoiminnot ovat keskeinen osa kaikkia ohjelmointikieliä, ja näin ollen, niitä löytyy myös useissa muissa Ruby-algoritmeissa ja -menetelmissä.