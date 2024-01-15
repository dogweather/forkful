---
title:                "Työskentely yaml:n kanssa"
html_title:           "Swift: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun pitäisi käyttää YAML-formaattia ohjelmoinnissa? YAML (YAML Ain't Markup Language) on helppolukuinen ja selkeä formaatti, joka on suosittu varsinkin ohjelmistokehittäjien keskuudessa. Se säästää aikaa ja vaivaa tiedostojen käsittelyssä ja tekee koodista helpommin ylläpidettävää.

## Kuinka

Jos haluat aloittaa käyttämään YAML-formaattia Swiftissä, seuraavassa on muutamia esimerkkejä siitä, kuinka voit tallentaa ja lukea tietoja YAML-muodossa:

```Swift
// Esimerkki YAML-tiedoston tallentamisesta
let dictionary = ["nimi": "Matti", "ikä": 25, "mieliharrastukset": ["lukeminen", "lenkkeily"]]
let data = try YAMLSerialization.data(withYAMLObject: dictionary)
try data.write(to: URL(fileURLWithPath: "tiedosto.yml"))

// Esimerkki YAML-tiedoston lukemisesta
let filePath = Bundle.main.path(forResource: "tiedosto", ofType: "yml")
let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
let dictionary = try YAMLSerialization.yamlObject(with: data) as? [String: Any]
print(dictionary["nimi"]) // Tulostaa "Matti"
```

## Syvemmälle

YAML-formaatti koostuu avaimista ja arvoista, kuten monissa muissakin ohjelmointikielissä. Avaimet ja arvot erotetaan kaksoispisteellä ja jokainen avain-arvo pari alkaa omalla rivillään. Alla olevassa esimerkissä näet, kuinka YAML-muodossa voi tallentaa monimutkaisempia tietorakenteita, kuten listoja ja sisäkkäisiä tietorakenteita.

```Swift
kotieläimet: # Tämä on avain
  - nimi: Haukku # Sisäkkäinen tietorakenne
    ikä: 3
  - nimi: Katti
    ikä: 5
    lempiruoka: kala
```

Lisätietoja YAML-formaatista ja sen syntaksista löytyy YAML-spesifikaatiosta. Voit myös käyttää monia erilaisia kirjastoja, kuten YAMLSerialization, SwiftyYAML tai Yams, helpottaaksesi YAML-tiedostojen käsittelyä Swiftissä.

## Katso myös

- [YAML-spesifikaatio](https://yaml.org/spec/)
- [YAMLSerialization-dokumentaatio](https://developer.apple.com/documentation/foundation/yamlserialization)
- [SwiftyYAML-repositorio](https://github.com/BeauNouvelle/SwiftyYAML)
- [Yams-repositorio](https://github.com/jpsim/Yams)