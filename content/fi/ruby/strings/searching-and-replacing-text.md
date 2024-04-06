---
date: 2024-01-20 17:58:31.216170-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) Ruby tekee etsimisest\xE4 ja korvaamisesta\
  \ helppoa. T\xE4ss\xE4 on pari esimerkki\xE4."
lastmod: '2024-04-05T21:53:58.654295-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) Ruby tekee etsimisest\xE4 ja korvaamisesta helppoa."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

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
