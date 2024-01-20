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

## Mitä ja miksi?
JSON (JavaScript Object Notation) on yleinen tapa tallentaa ja vaihtaa tietoja ohjelmoinnissa. Se on yksinkertainen formaatti, joka käyttää avain-arvo -pareja tallentaakseen tietoja ja on erityisen hyödyllinen verkkosovelluksissa, kun tietoja tarvitsee lähettää ja vastaanottaa nopeasti. Tämän takia moni ohjelmoija käyttää JSONia osana päivittäistä työtään.

## Miten?
Jos haluat luoda validin JSON -muotoisen tiedoston Swiftillä, sinun tarvitsee vain muokata datanavakkaa (struct) niin, että se sisältää tarvittavat avain-arvo -parit. Jälkeenpäin voit käyttää JSONSerialization -luokkaa muuntamaan datanavakan JSONiksi ja JSON -sta datanavakaksi. Katso allaolevia esimerkkejä.

```Swift
// Luodaan datanavakka party

struct Party {
    let guests: [String]
    let theme: String
}

// ALustetaan datanavakka

let party = Party(guests: ["Maija", "Matti", "Hanna"], theme: "Retro")

// Muunnetaan datanavakka JSONiksi

var jsonData: Data
do {
    jsonData = try JSONSerialization.data(withJSONObject: party, options: .prettyPrinted)
} catch {
    print(error)
}

// Muunnetaan JSON datanavakaksi 

var convertedParty: Party
do {
    let json = try JSONSerialization.jsonObject(with: jsonData, options: [])
    if let jsonParty = json as? [String: Any] {
        convertedParty = Party(guests: jsonParty["guests"][0] as! String, theme: jsonParty["theme"] as! String)
    }
} catch {
    print(error)
}

print(convertedParty.guests) // tulostaa ["Maija", "Matti", "Hanna"]
print(convertedParty.theme) // tulostaa "Retro"
```

## Syvempi sukellus
JSON luotiin alunperin selain- ja JavaScript -yhteensopivaksi tiedonsiirtoformaadiksi. Sitä kutsuttiin aluksi "Jaavscript -ohjelmien maitojauheeksi" ja sen käyttöönotto 90-luvun lopulla auttoi selaimia lähettämään ja vastaanottamaan tietoja nopeammin ja tehokkaammin. Nykyään JSONia käytetään paljon yleisemmin monenlaisissa ohjelmointikielissä ja sen suosio kasvaa jatkuvasti.

On myös muita vaihtoehtoja JSONille, kuten XML ja YAML, mutta JSON on yleensä nopeampi ja helpompi käyttää. Swiftin lisäksi myös muut ohjelmointikielet, kuten JavaScript, Python ja Ruby, tukevat JSONia.

Jos haluat käyttää JSONia Swiftissä, tarvitset lisäksi JSONSerialization -luokan lisäksi myös Foundation -kirjaston. Voit lukea lisää JSONSerializationista ja sen eri metodeista Swiftin dokumentaatiosta.

## Lisätietoa
- [Swiftin dokumentaatio JSONSerializationista](https://developer.apple.com/documentation/foundation/jsonserialization)
- [JSON-formatin virallinen verkkosivu](https://www.json.org/)