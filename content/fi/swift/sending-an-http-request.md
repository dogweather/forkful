---
title:                "Lähettää http-pyyntö"
html_title:           "Swift: Lähettää http-pyyntö"
simple_title:         "Lähettää http-pyyntö"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyyntöjä? Usein haluamme noutaa tietoa verkkopalvelimelta, esimerkiksi hakemalla uusimmat tiedot sovellukseen, päivittämällä palvelimen tietokannan tai lähettämällä tietoa käyttäjän antamaa lomaketta varten.

## Kuinka

```Swift
let url = URL(string: "https://example.com")

var request = URLRequest(url: url)
request.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: request) { (data, response, error)in
    if let error = error {
        print("Virhe: \(error)")
    } else if let responseData = data {
        print("Vastaus: \(responseData)")
    }
}

task.resume()
```

 *Keskimääräinen koodiesimerkki lähettää GET-pyynnön URL-osoitteeselle ja tulostaa vastauksen, jos pyyntö onnistuu.*

```Swift
let url = URL(string: "https://example.com")

var request = URLRequest(url: url)
request.httpMethod = "POST"
request.httpBody = "data=exampleData".data(using: .utf8)

let task = URLSession.shared.dataTask(with: request) { (data, response, error)in
    if let error = error {
        print("Virhe: \(error)")
    } else if let responseData = data {
        print("Vastaus: \(responseData)")
    }
}

task.resume()
```

 *Toinen esimerkki lähettää POST-pyynnön URL-osoitteeseen ja liittää mukaan tiedon JSON-muodossa.*

## Syväsukellus

HTTP-pyyntöjä lähetettäessä on tärkeä ottaa huomioon muutamia asioita. Ensinnäkin, pyyntöjen lähettämiseen käytetään yleensä URL-luokkaa ja URLRequest-luokkaa. Lisäksi on hyvä tietää, että pyynnön tyyppi määritellään httpMethod -muuttujalla ja mahdollinen lisätieto lähetetään httpBody -muuttujalla. Lopuksi pyyntö lähetetään käyttämällä URLSession-luokkaa ja kutsutaan dataTask-metodia, joka käynnistää itse pyynnön.

## Katso myös

[HTTP-pyynnöt Swiftillä](https://www.raywenderlich.com/158106/urlsession-tutorial-getting-started)

[Apple:n dokumentaatio HTTP-pyyntöjen lähettämiseen](https://developer.apple.com/documentation/foundation/urlloadingystem)