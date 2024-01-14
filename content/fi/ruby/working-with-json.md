---
title:                "Ruby: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yksi suosituimmista tietojen tallennusmuodoista nykyään. Se on kevyt, helppolukuinen ja helppo käyttää. Monet nykyaikaiset ohjelmointikielet, kuten Ruby, tarjoavat tuen JSON-tiedostojen käsittelyyn. Jos haluat olla pätevä ohjelmoija, on tärkeää oppia työskentelemään JSON:in kanssa.

## Miten

Ruby tarjoaa sisäänrakennetut metodit JSON-tietojen käsittelyyn. Aloita luomalla uusi tiedosto ja kirjoittamalla sinne seuraava koodi:

```Ruby
require 'json'

data = { nimi: 'Matti', ikä: 30, kaupunki: 'Helsinki' }
```

Ensimmäinen rivi tuo Ruby:n JSON-kirjaston käyttöön, jotta voimme käyttää JSON-metodeja. Toisella rivillä luomme muuttujan "data", joka sisältää avain-arvo -pareja. Tässä esimerkissä käytämme henkilön nimeä, ikää ja kaupunkia.

Seuraavaksi voimme käyttää JSON-metodeja muuttaaksemme datan JSON-muotoon ja tallentaa sen tiedostoon:

```Ruby
File.open('henkilot.json','w') do |f|
  f.write(data.to_json)
end
```

Tämä luo uuden tiedoston nimeltä "henkilot.json" ja tallentaa siihen datan JSON-muodossa. Voit tarkistaa tiedoston sisällön avaamalla sen tekstieditorilla.

Jos haluat lukea JSON-tiedoston takaisin Ruby:n muuttujaksi, voit käyttää seuraavaa koodia:

```Ruby
File.open('henkilot.json') do |f|
  parsed_data = JSON.parse(f.read)
end
```

Nyt voit käyttää muuttujaa "parsed_data" samalla tavalla kuin aiemmin luomaasi "data" -muuttujaa.

## Syvennys

JSON:ssa on myös mahdollista luoda monimutkaisempia rakenteita, kuten sisäkkäisiä avain-arvo -pareja ja listoja. Voit tutustua lisää JSON:n syntaksiin ja mahdollisuuksiin täältä: https://www.json.org/json-fi.html.

Voit myös käyttää Ruby:n "pretty_generate" -metodia tulostamaan JSON-tiedoston kauniissa muodossa:

```Ruby
puts JSON.pretty_generate(parsed_data)
```

## Katso myös

- JSON:n virallinen verkkosivusto: https://www.json.org/json-fi.html
- Ruby:n dokumentaatio JSON-kirjastolle: https://ruby-doc.org/stdlib-3.0.0/libdoc/json/rdoc/JSON.html