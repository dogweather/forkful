---
title:                "Työskentely jsonin kanssa"
html_title:           "Gleam: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
JSON on datan tallennus- ja siirtomuoto, jota käytetään usein ohjelmistokehityksessä. Ohjelmoijat käyttävät JSONia helposti luettavan ja muokattavan datan tallentamiseen ja jakamiseen.

## Kuinka:
Gleamin avulla voit luoda ja lukea JSON-muotoista dataa käyttämällä ```Gleam.Json``` -moduulia. Voit luoda JSON-merkkijonoja käyttämällä ```Gleam.Json.encode``` -funktiota ja lukea JSON-tietoja käyttämällä ```Gleam.Json.decode``` -funktiota. Esimerkiksi:
```Gleam
let data = {name: "Tommi", age: 25}
let json_string = Gleam.Json.encode(data)
// Tulostaa: {"name":"Tommi","age":25}
io.println(json_string)
let decoded_data = Gleam.Json.decode(json_string)
// Tulostaa: {Ok,{"name":"Tommi","age":25}}
io.println(show(decoded_data))
```

## Syvällinen sukellus:
JSON, lyhenne sanoista JavaScript Object Notation, kehitettiin alunperin JavaScript-kielen yhteyteen, mutta siitä on tullut suosittu dataformaatti myös muiden ohjelmointikielten keskuudessa. JSON koostuu avain-arvo pareista, jotka ovat järjestyksessä oleva lista avaimia ja niihin liittyviä arvoja. Vaikka JSON on suosittu, on olemassa myös muita vaihtoehtoja datan tallentamiseen ja siirtämiseen, kuten XML tai CSV. Gleam tukee myös näiden formaattien käsittelyä useiden moduulien avulla, kuten ```Gleam.Xml``` ja ```Gleam.Csv```.

## Katso myös:
- Gleamin virallinen dokumentaatio: https://gleam.run
- JSON-esittely W3 Schools-sivustolla: https://www.w3schools.com/js/js_json_intro.asp
- Vertailu JSONin ja muiden formaattien välillä: https://www.json.org/json-fi.html