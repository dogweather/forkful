---
title:                "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle"
html_title:           "Ruby: Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle"
simple_title:         "Tietokoneohjelmoinnin artikkeli: Kirjoittaminen standardivirheelle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standard erroriin on hyödyllinen tapa ilmoittaa virheistä ja varoituksista ohjelman suorituksen aikana. Tämä auttaa kehittäjiä tunnistamaan ja korjaamaan mahdollisia ongelmia nopeammin.

## Kuinka

Kirjoittaminen standard erroriin Rubyssa on helppoa. Se onnistuu käyttämällä `STDERR.puts` tai `STDERR.print` -komennon sijaan perinteisiä `puts` ja `print` -komentoja.

```Ruby
# Kirjoitetaan virheviesti standard erroriin
STDERR.puts "Virhe: Ei löydy tiedostoa"

# Kirjoitetaan varoitussanoma standard erroriin
STDERR.print "Varoitus: Tiedosto saattaa olla korruptoitunut"
```

Tulostuksena saadaan:

```
Virhe: Ei löydy tiedostoa
Varoitus: Tiedosto saattaa olla korruptoitunut
```

## Syvemmälle

Kirjoittaminen standard erroriin on välttämätöntä silloin, kun halutaan erottaa virheviestit ja varoitukset normaalista tulosteesta. Jos esimerkiksi käytämme `puts`-komentoa, viesti tulostuu standard ulostuloon ja sekoittuu mahdollisiin muihin tulosteisiin.

Lisäksi voi olla hyötyä muokata standard errorin ulkoasua. Tämä on mahdollista esimerkiksi värikoodien ja muotoilujen avulla. Alla olevassa esimerkissä käytämme `colorize` -kirjastoa, joka mahdollistaa tekstin muotoilun Rubyssa.

```Ruby
require 'colorize'

# Kirjoitetaan virheviesti standard erroriin käyttäen punaista väriä ja lihavointia
STDERR.puts "Virhe: Ei löydy tiedostoa".red.bold

# Kirjoitetaan varoitussanoma standard erroriin käyttäen keltaista väriä ja alleviivausta
STDERR.print "Varoitus: Tiedosto saattaa olla korruptoitunut".yellow.underline
```

Tulostuksena saadaan:

```
Virhe: Ei löydy tiedostoa
Varoitus: Tiedosto saattaa olla korruptoitunut
```

## Katso myös

- [Ruby Dokumentaatio](https://ruby-doc.org/core-3.0.0/IO.html#class-IO-label-Printing+z+directly+to+the+tty)
- [Ruby Colorize-kirjasto](https://github.com/fazibear/colorize)