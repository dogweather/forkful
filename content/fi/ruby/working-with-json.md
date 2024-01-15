---
title:                "Työskentely jsonin kanssa"
html_title:           "Ruby: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää JSONia Ruby-ohjelmoinnissa? JSON eli JavaScript Object Notation on suosittu tiedonvälitysmuoto verkossa ja monissa ohjelmointikielissä, joten sen tunteminen hyödyllistä työssäsi ohjelmoijana.

## Kuinka

JSONin käsittely Rubyssa on helppoa ja suoraviivaista. Käytämme JSON-pakettia, joka sisältää valmiit työkalut JSON-tietojen parsimiseen ja generoimiseen. Katso seuraava koodiesimerkki JSON-tiedon parsimisesta:

```Ruby
require 'json'

# JSON-dataa, jonka haluamme parsia
json_data = '{"nimi": "Anna", "kaupunki": "Helsinki", "ikä": 30}'

# Parsitaan JSON-data ja tallennetaan se muuttujaan
tulos = JSON.parse(json_data)

# Printataan tuloksen avain ja arvo
puts "Nimi: #{tulos['nimi']}\nKaupunki: #{tulos['kaupunki']}\nIkä: #{tulos['ikä']}"

```

Tulostus:

```
Nimi: Anna
Kaupunki: Helsinki
Ikä: 30
```

Havaitsemme, että JSON-data on nyt purettu ja tallennettu muuttujaan. Voimme nyt käyttää sitä ohjelmassamme haluamallamme tavalla. Seuraavaksi esittelemme vaikeampia JSON-käsittelyn tapauksia.

## Syväsukellus

JSON-tiedosto voi sisältää monimutkaisempia rakenteita, kuten sisäkkäisiä objekteja ja listoja. Tässä tapauksessa voimme käyttää Ruby-kieleen sisäänrakennettua Hash-metodia, joka tekee JSON-objektin käsittelystä helpompaa. JSON-tiedoston käsittelyyn sisältyy myös virheiden käsittely, kuten esimerkiksi virheellisen tiedon parsiminen. Voit oppia lisää tästä Ruby'n virallisen dokumentaation avulla.

## Katso myös

- Virallinen Ruby JSON-dokumentaatio: https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html
- JSON-opas: https://www.tutorialspoint.com/ruby-json-tutorial