---
title:                "Työskentely jsonin kanssa"
html_title:           "Swift: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi JSON:lla kannattaa työskennellä?

JSON (JavaScript Object Notation) on yksi yleisimmin käytetty formaatti tiedon vaihtamiseen eri sovellusten välillä. Se on helppo lukea ja parantaa tiedon rakenteellista muotoa. JSON:ia käytetään usein etenkin REST API-palveluiden kommunikoinnissa.

## Kuinka käyttää JSON:ia Swiftin kanssa?

Käytä `JSONSerialization`-luokkaa muuntaaksesi `Data`-objektin JSON-muotoon ja `JSONDecoder`-luokkaa deserialisoimaan `Data`-objektin JSON-muodosta takaisin Swiftin objekteiksi. Tämän jälkeen voit käyttää `Dictionary`- ja `Array`-tyyppejä saadaksesi tiedot JSON-muodosta.

Esimerkiksi:

```Swift
let json = """
{
    "person": {
        "name": "Matti Meikäläinen",
        "age": 27
    }
}
""".data(using: .utf8)!

if let jsonDictionary = try JSONSerialization.jsonObject(with: json, options: []) as? [String: Any],
   let person = jsonDictionary["person"] as? [String: Any],
   let name = person["name"] as? String,
   let age = person["age"] as? Int {
   print("\(name) on \(age) vuotta vanha.")
} else {
   print("Virhe: Ei voitu lukea JSON-muotoa.")
}

// Tulostaa: "Matti Meikäläinen on 27 vuotta vanha."
```

## JSON:n syväluotausta

 JSON:n avulla voit tallentaa monimutkaisia tietorakenteita, kuten `Dictionary`s ja `Array`s, ja käyttää niitä helposti eri sovellusten välillä. JSON:in käyttö on myös turvallista, sillä se ei suorita koodia, mikä estää mahdolliset tietoturvariskit.

## Katso myös
- [Apple:n JSON-ohjeet Swiftillä](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Swiftin JSONDecoder-dokumentaatio](https://developer.apple.com/documentation/foundation/jsondecoder)
- [JSON:n opiskelu: koodin-esimerkkejä Swiftillä](https://www.raywenderlich.com/1525015-swift-json-tutorial-getting-started)