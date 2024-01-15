---
title:                "Työskentely jsonin kanssa."
html_title:           "Gleam: Työskentely jsonin kanssa."
simple_title:         "Työskentely jsonin kanssa."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON on tällä hetkellä yksi markkinoiden suosituimmista tiedonsiirtomuodoista. Se on yksinkertainen ja tehokas tapa välittää tietoja sovellusten välillä, joten jos haluat pystyä työskentelemään tämän päivän ohjelmistomaailmassa, sinun kannattaa olla perillä JSONista.

## Kuinka tehdä

Gleam tarjoaa helpon ja tehokkaan tavan työskennellä JSONin kanssa. Voimme käyttää internetyhteyskirjastoa ja integroida sen Gleamiin seuraavalla tavalla:

```
Gleam.HTTP.request(
  url = "https://jsonplaceholder.typicode.com/todos/1",
  method = "GET",
  headers = [],
  body = Gleam.HTTP.empty_body(),
  expect = Gleam.HTTP.expect_json(Todo.decode)
)
```

Tässä koodissa lähetämme GET-pyynnön URL-osoitteeseen ja odotamme vastaavan JSON-datan kääntämistä Gleamin Todo-tietorakenteeksi. Voimme myös käyttää Gleamin sisäänrakennettua taulukkotyyppiä ja tehdä tietojen käsittelystä helpompaa:

```
let json = """{"name": "John", "age": 30, "tags": ["programming", "Gleam"]}"""
let data = String.to_json(json)
if Array.member(String.to_upper(String.to_utf8(data.tags)), "GLEAM") {
  io.println("Tämä henkilö on Gleam-fani!")
}
```

Tässä esimerkissä käytämme String-toimintoja muuttamaan JSON-data json-tietotyypiksi. Sitten voimme käyttää taulukoita ja merkkijonojen käsittelyfunktioita helposti datan tarkastelussa.

## Syvällinen sukellus

JSONin kanssa työskentely Gleamilla on helppoa ja intuitiivista. Voit helposti muuntaa JSON-datan oman sovelluksesi tietorakenteeksi ja käsitellä sitä Gleamin sisäänrakennetuilla toiminnoilla. Voit myös käyttää Gleamin tyyppijärjestelmää varmistamaan, että kaikki tiedot vastaavat odotettua tietotyyppiä.

See Also 

- [Gleam JSON-dokumentaatio](https://gleam.run/articles/json)
- [Gleam internetyhteyskirjasto](https://hexdocs.pm/gleam/Gleam.HTTP.html)
- [Gleam tietotyypit](https://hexdocs.pm/gleam/Gleam.html#types)