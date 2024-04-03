---
date: 2024-01-20 17:58:31.216170-07:00
description: "Tekstin etsiminen ja korvaaminen tarkoittaa jonkin merkkijonon l\xF6\
  yt\xE4mist\xE4 ja muuttamista toiseksi. Ohjelmoijat k\xE4ytt\xE4v\xE4t t\xE4t\xE4\
  \ toimintoa p\xE4ivitt\xE4ess\xE4\xE4n\u2026"
lastmod: '2024-03-13T22:44:57.070382-06:00'
model: gpt-4-1106-preview
summary: "Tekstin etsiminen ja korvaaminen tarkoittaa jonkin merkkijonon l\xF6yt\xE4\
  mist\xE4 ja muuttamista toiseksi."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## What & Why? (Mitä ja miksi?)
Tekstin etsiminen ja korvaaminen tarkoittaa jonkin merkkijonon löytämistä ja muuttamista toiseksi. Ohjelmoijat käyttävät tätä toimintoa päivittäessään koodia, muokatessaan dataa tai tehdessään massamuutoksia.

## How to: (Kuinka tehdään:)
Ruby tekee etsimisestä ja korvaamisesta helppoa. Tässä on pari esimerkkiä:

```Ruby
# Perus stringin korvaus
teksti = "Kissa juoksee nopeasti"
uusi_teksti = teksti.gsub('nopeasti', 'hitaasti')
puts uusi_teksti
# Tulostaa "Kissa juoksee hitaasti"

# Säännöllisten lausekkeiden käyttö monimutkaisempaan hakuun
sähköposti = "esimerkki@osoite.fi"
anonimoitu_sposti = sähköposti.gsub(/[^@]+/, '******')
puts anonimoitu_sposti
# Tulostaa "******@osoite.fi"
```

## Deep Dive (Sukellus syvyyksiin):
Tekstin etsiminen ja korvaaminen juontaa juurensa tekstinkäsittelyohjelmista – se oli välttämättömyys pitkien dokumenttien muokkaamisessa. Nykyään ohjelmointikieli Ruby tarjoaa `.gsub` ja `.sub` metodeja merkkijonojen käsittelyyn. `.gsub` etsii ja korvaa kaikki esiintymät, kun taas `.sub` korvaa vain ensimmäisen. Säännölliset lausekkeet (regex) tarjoavat voimakkaan tavan hakuun, ne mahdollistavat monimutkaiset ehdot ja merkkiryhmät.

Alternative tapoja tekstinkäsittelyyn ovat komentorivillä toimivat työkalut kuten `sed` ja `awk` Unix-pohjaisissa järjestelmissä, tai vastaavat toiminnot tekstieditoreissa ja kehitysympäristöissä.

## See Also (Katso myös):
- [Ruby-doc for String#gsub and String#sub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- [Rubular, a Ruby regular expression editor](http://rubular.com/)
- [The Bastards Book of Regexes, an introduction to regular expressions](http://ruby.bastardsbook.com/chapters/regexes/)
