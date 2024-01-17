---
title:                "Lähettämällä http-pyyntö"
html_title:           "Swift: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettäminen HTTP-pyyntö on tapa, jolla ohjelmoijat voivat kommunikoida verkon kautta muiden palvelimien kanssa. Tämä voi sisältää pyyntöjä, kuten tietojen hakemista tai lähettämistä ja voi auttaa sovelluksiasi kommunikoimaan muiden palveluiden kanssa.

## Kuinka tehdä se:
```Swift
// Luodaan URL-osoite
let url = URL(string: "https://www.example.com")

// Luodaan HTTP-pyyntö
var request = URLRequest(url: url!)
request.httpMethod = "GET"

// Tehdään pyyntö
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "Unknown error")
        return
    }

    // Tulostetaan saatu vastaus
    if let response = response {
        print(response)
    }

    // Muokataan vastausta haluttuun muotoon
    if let dataString = String(data: data, encoding: .utf8) {
        print(dataString)
    }
}

// Käynnistetään pyyntö
task.resume()
```
Lopputulos on HTTP-pyynnön vastaus ja mahdolliset virheet konsolissa.

## Syvällisempi sukellus:
HTTP-pyyntöjen lähettämistä käytetään laajalti verkko-ohjelmoinnissa ja se perustuu HTTP-protokollaan, joka on ollut olemassa vuodesta 1991 lähtien. On myös olemassa muita tapoja kommunikoida verkon kanssa, kuten REST-apit ja WebSocketit. Swiftissä HTTP-pyyntöjen lähettämiseen käytetään perinteisesti URLSessionia, mutta myös muita kirjastoja, kuten Alamofire ja Moya, ovat suosittuja vaihtoehtoja.

## Katso myös:
- [NSURLSession-tekninen dokumentaatio](https://developer.apple.com/documentation/foundation/urlsession)
- [Alamofire:n GitHub-sivut](https://github.com/Alamofire/Alamofire)
- [Moya:n GitHub-sivut](https://github.com/Moya/Moya)