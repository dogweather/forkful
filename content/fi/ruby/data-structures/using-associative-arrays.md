---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:39.973175-07:00
description: "Assosiatiiviset taulukot, jotka tunnetaan yleisemmin hasheina Rubyn\
  \ kieless\xE4, mahdollistavat ainutlaatuisten avainten parittamisen arvoihin. Ne\
  \ ovat\u2026"
lastmod: '2024-03-13T22:44:57.077755-06:00'
model: gpt-4-0125-preview
summary: "Assosiatiiviset taulukot, jotka tunnetaan yleisemmin hasheina Rubyn kieless\xE4\
  , mahdollistavat ainutlaatuisten avainten parittamisen arvoihin."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Miten:
Hashien luominen ja käyttö Rubyn kielessä on suoraviivaista. Voit alustaa tyhjän hashin, täyttää sen avain-arvo -pareilla, päästä käsiksi arvoihin niiden avainten kautta ja paljon muuta. Tässä miten teet sen:

```Ruby
# Hashin luominen
my_hash = { "name" => "John Doe", "age" => 30 }

# Toinen tapa luoda hash
another_hash = Hash.new
another_hash["position"] = "Developer"

# Hash-arvojen käyttö
puts my_hash["name"] # Tuloste: John Doe

# Uuden avain-arvo -parin lisääminen
my_hash["language"] = "Ruby"
puts my_hash # Tuloste: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Hashin läpikäyminen
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Tuloste:
# name: John Doe
# age: 30
# language: Ruby
```

Voit myös käyttää symboleita tehokkaampina avaimina:

```Ruby
# Käyttäen symboleita avaimina
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Tuloste: Jane Doe
```

## Syventävä osio:
Assosiatiivisten taulukoiden konsepti ei ole ainutlaatuinen Rubylle; monet kielet toteuttavat ne eri nimillä, kuten sanakirjat Pythonissa tai objektit JavaScriptissä (kun niitä käytetään avain-arvo -pareina). Rubyn alkuaikoina hashit olivat hieman hitaampia eivätkä niin monipuolisia. Kuitenkin ajan myötä, Rubyn hashien toteutus on optimoitu erittäin tehokkaaksi, erityisesti symboliavaimille, tehden niistä erittäin tehokkaita usein tapahtuviin haku- ja päivitystoimintoihin.

Rubyn hashit erottuvat niiden syntaktisen helppokäyttöisyyden ja joustavuuden ansiosta - voit käyttää lähes mitä tahansa objektityyppiä avaimena, vaikkakin symbolit ja merkkijonot ovat yleisimpiä. Sisäisesti, Rubyn hashit toteutetaan käyttäen hajautusalgoritmia, joka tasapainottaa nopeutta ja muistitehokkuutta, jopa kun elementtien määrä kasvaa.

Vaikka hashit ovat uskomattoman monikäyttöisiä, ne eivät ole kaikenkattava ratkaisu datan säilytykseen Rubyn kielessä. Järjestettyihin kokoelmiin taulukot ovat sopivampia, ja uniikkien esineiden joukoille Set saattaisi olla parempi valinta. Lisäksi, erittäin monimutkaisten tietorakenteiden kohdalla, omien luokkien luominen saattaisi olla suositeltavaa.

Muista, että valinta käyttää hashia versus muita tietorakenteita tulee suurelta osin alas tietyssä käyttötapauksessa—hashit excelsoituvat nopeissa hauissa ja ylläpitävät yhdistyksiä uniikkien avainten ja niiden arvojen välillä.
