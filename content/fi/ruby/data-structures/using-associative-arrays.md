---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases: - /fi/ruby/using-associative-arrays.md
date:                  2024-01-30T19:12:39.973175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot, jotka tunnetaan yleisemmin hasheina Rubyn kielessä, mahdollistavat ainutlaatuisten avainten parittamisen arvoihin. Ne ovat korvaamattomia, kun tarvitset pitää kirjaa elementeistä tietyn viitteen kautta, kuten säilyttämään olion ominaisuuksia tai nopeasti pääsemään käsiksi tietoihin uniikin tunnisteen avulla.

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
