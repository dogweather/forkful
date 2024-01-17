---
title:                "Lähetetään http-pyyntö perusautentikoinnilla"
html_title:           "Swift: Lähetetään http-pyyntö perusautentikoinnilla"
simple_title:         "Lähetetään http-pyyntö perusautentikoinnilla"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Perustason todennustietojen lähettäminen HTTP-pyynnössä tarkoittaa käyttäjän tunnistamisessa tarvittavan tiedon lähettämistä palvelimelle. Tieto sisältää yleensä käyttäjän käyttäjänimen ja salasanan. Tätä tehdään ohjelmoijien toimesta turvallisemman käyttökokemuksen varmistamiseksi.

## Kuinka:

```Swift
// Luodaan URLRequest-objekti ja annetaan sille osoite ja menetelmä
let urlString = "https://www.example.com/login" 
if let url = URL(string: urlString) {
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    
    // Lisätään käyttäjänimen ja salasanan Base64-enkoodattu versio Authorization-headeriin
    let username = "käyttäjänimi"
    let password = "salasana"
    let loginString = String(format: "%@:%@", username, password)
    if let loginData = loginString.data(using: String.Encoding.utf8) {
        let base64LoginString = loginData.base64EncodedString()
        request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
    }
    
    // Luodaan ja lähetetään pyyntö käyttäen URLSession:ää
    let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
        if let error = error {
            print("Virhe: \(error)")
        } else if let data = data {
            print("Saatu data: \(data)")
            if let responseString = String(data: data, encoding: .utf8) {
                print("Vastaus: \(responseString)")
            }
        }
    }
    task.resume()
}
```

###Esimerkkilähtö ja -tulos:

```Swift
// Lähetetään POST-pyyntö osoitteeseen www.example.com/login käyttäjänimellä ja salasanalla ohjelman käyttäjän antamiksi.
// Pyynnön vastauksena saadaan esimerkkidataa.

Saatu data: Optional(18 bytes)
Vastaus: HereIsYourData!

```

## Syväsukellus:

Perustason todennustietojen lähettämisellä HTTP-pyynnössä on juurensa internetin alkuajoista, jolloin tarvittiin tapa varmistaa, että vain valtuutetut käyttäjät pystyivät käyttämään tiettyjä verkkosivustoja tai -palveluja.

Vaihtoehtoisia tapoja lähettää perustasoisen todennustiedon lisäksi myös muitakin tietoja ovat esimerkiksi Digest Authentication ja OAuth. Perustasoinen todennus on kuitenkin edelleen käytössä monien verkkopalveluiden ja -sovellusten turvallisuuden varmistamiseksi.

HTTP Basic Authentication toimii lähettämällä käyttäjänimen ja salasanan Base64-enkoodattuina pyynnön otsikkotietoina. Tämä tapahtuu käyttämällä määritettyä "Authorization" -headeria, joka sisältää merkinnän "Basic" ja enkoodatut tiedot.

## Katso myös:

- [URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Base64 Encoding in Swift](https://stackoverflow.com/questions/47645595/base64-encoding-in-swift-4/47645759)