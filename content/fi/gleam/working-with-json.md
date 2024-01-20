---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
JSON on dataformaatti tiedon tallennukseen ja vaihtoon. Käytämme sitä koska se on kevyt, lukee ihmisen silmälle ja koneille, ja se on yhteensopiva lähes kaikkien ohjelmointikielten kanssa.

## Miten:
```gleam
import gleam/should
import gleam/json

pub fn example_json_usage() {
  // Luodaan JSON-olio
  let data = json.object([
    tuple("nimi", json.string("Matti")),
    tuple("ikä", json.int(30)),
    tuple("on_koodaaja", json.bool(true)),
  ])

  // Muunnetaan JSON-olio merkkijonoksi
  let json_string = json.encode(data)

  // Tulosta JSON-merkkijonon
  should.equal(json_string, Ok("{\"nimi\":\"Matti\",\"ikä\":30,\"on_koodaaja\":true}"))
  
  // Parsi JSON-merkkijono takaisin JSON-olioksi
  let parsed_data = json.decode(json_string)
  should.equal(parsed_data, Ok(data))
}
```

## Syväkatsaus
JSON on kehitetty alun perin JavaScript-ohjelmointikielen osana, mutta se on irrotettu ja standardoitu itsenäisenä datamuotona. Vaihtoehtoisia formaatteja ovat mm. XML ja YAML. Gleamissa JSONin käsittely perustuu `gleam/json`-kirjastoon, joka tarjoaa tyypitetyn käyttöliittymän JSONin enkoodaamiseen ja dekoodaamiseen.

## Katso Myös
- JSONin virallinen määrittely: [JSON.org](http://json.org/)
- Gleam kielen kotisivu: [Gleam-lang](https://gleam.run/)