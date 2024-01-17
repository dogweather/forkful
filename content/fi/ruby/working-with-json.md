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

## Mikä & Miksi?

JSON (JavaScript Object Notation) on tiedostomuoto, jota käytetään tiedon tallentamiseen ja siirtämiseen. JSON on suosittu ohjelmoijien keskuudessa, koska se on helppo luettavuutensa ja yksinkertaisen syntaksinsa ansiosta.

## Miten:

Esimerkiksi, jos haluamme tallentaa käyttäjän tietoja JSON-muodossa, voimme käyttää seuraavaa koodia:

```Ruby
require 'json'

user = { name: "Matti", age: 25, email: "matti@example.com" }

json_user = JSON.generate(user)

puts json_user
```

Tulostus:

```
{"name":"Matti","age":25,"email":"matti@example.com"}
```

Tässä koodissa käytämme Ruby:n JSON-kirjastoa, jonka avulla voimme helposti muuntaa Ruby-olion JSON-muotoon käyttämällä `JSON.generate` -metodia.

Voimme myös muuntaa JSON-muotoisen tiedoston takaisin Ruby-olioksi käyttämällä `JSON.parse` -metodia:

```Ruby
require 'json'

json_user = '{"name":"Matti","age":25,"email":"matti@example.com"}'

user = JSON.parse(json_user)

puts user[:name]
puts user[:age]
```

Tulostus:

```
Matti
25
```

## Syvällinen sukellus:

JSON kehitettiin alunperin JavaScript-kielellä, mutta sitä käytetään nykyään laajalti monilla eri ohjelmointikielillä. JSON on myös saanut suosiota sen yksinkertaisuuden ja helppokäyttöisyyden ansiosta verrattuna muihin tiedon tallennusmuotoihin, kuten XML:ään.

JSON:ia käytetään usein tiedonsiirtoprotokollassa, kuten REST-rajapinnoissa ja Ajax-sovelluksissa. JSON-muoto soveltuu hyvin myös web-sovellusten backendin ja frontendin väliseen kommunikaatioon.

## Katso myös:

- JSON:n viralliset verkkosivut: https://www.json.org/
- Ruby:n JSON-kirjaston dokumentaatio: https://ruby-doc.org/stdlib/libdoc/json/rdoc/JSON.html