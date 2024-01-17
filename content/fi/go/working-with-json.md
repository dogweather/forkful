---
title:                "Työskentelyä jsonin kanssa"
html_title:           "Go: Työskentelyä jsonin kanssa"
simple_title:         "Työskentelyä jsonin kanssa"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
JSON (JavaScript Object Notation) on kevyt ja suosittu tiedonsiirtomuoto ohjelmoinnissa. Se mahdollistaa tietojen tallentamisen ja jakamisen selkeässä ja helposti luettavassa muodossa, joka on ymmärrettävä monille ohjelmointikielille. Pääsyynä JSONin käyttöön on sen yksinkertaisuus ja käytännöllisyys, mikä helpottaa monimutkaisenkin datan käsittelyä.

## Miten:
Ensimmäinen askel JSONin käytössä Go-ohjelmoinnissa on tuoda kirjasto käyttöön import-komennolla. Tämän jälkeen voidaan käyttää paketin json.Encode- ja json.Decode-funktioita, jotka muuntavat tiedon automaattisesti JSON-muotoon ja takaisin. Katso alla oleva koodiesimerkki ja sen tulostus.

```Go
import "encoding/json"

// Muuttujatietojen tallentaminen JSON-muotoon
data := map[string]interface{}{"nimi": "Matti", "ika": 25}
jsonData, _ := json.Encode(data)

// Tulostaminen konsoliin
fmt.Println(jsonData)
```
```Go
// Tulostus: {"nimi":"Matti", "ika":25}
```

## Syvällistä tietoa:
JSON kehitettiin alun perin JavaScript-kielelle, mutta se on nykyään yleisesti käytössä monissa muissa kielissä, kuten Go:ssa. JSONia käytetään usein REST API -rajapintojen tietojen siirrossa, mutta myös esimerkiksi tallennusmuotona tietokannoissa. Vaihtoehtoisesti XML-muotoa voidaan käyttää samoihin tarkoituksiin, mutta JSONin suosiota ajettiin sen selkeydellä ja yksinkertaisuudella.

## Katso myös:
- [Go-opetusohjelma JSON-muotoinen API: n luomiseen](https://tutorialedge.net/golang/go-json-tutorial/)
- [Yleiskatsaus JSON:iin ja sen käyttöön Go:ssa](https://golang.org/pkg/encoding/json/)