---
title:                "Http-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Swift: Http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "Http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi lähettää HTTP-pyynnön käyttäen perusautentikointia? Perusautentikointi on yksi tapa suojata web-palveluita ja sen käyttö voi olla hyödyllistä esimerkiksi API-kutsuissa.

## Kuinka
```Swift
let urlString = "https://www.example.com"
let url = URL(string: urlString)

// Luodaan request ja lisätään otsikko "Authorization" käyttäen perusautentikointia
var request = URLRequest(url: url!)
let loginData = String(format: "%@:%@", "käyttäjänimi", "salasana").data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Luodaan URLSession ja lähetetään pyyntö
let session = URLSession.shared
let task = session.dataTask(with: request) { (data, response, error) in
  if let error = error {
    print("Virhe: \(error)")
  } else if let data = data {
    // Tulostetaan vastaus ja pakataan se JSON-muotoon
    print("Vastaus: \(String(data: data, encoding: .utf8)!)")
    let json = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
    print("JSON: \(json)")
  }
}
task.resume()
```

Koodiesimerkissä näytetään, kuinka tehdä HTTP-pyyntö käyttäen perusautentikointia Swift-ohjelmointikielellä. Ensin luodaan URL ja sitten Request-objekti, johon lisätään otsikko "Authorization" käyttäen perusautentikointia. Lopuksi luodaan URLSession ja lähetetään pyyntö. Jos kaikki menee hyvin, vastaus tulostetaan ja pakataan JSON-muotoon.

## Syventävää
Perusautentikointi on yksi vanhimmista tavoista suojata web-palveluita. Se toimii lähettämällä käyttäjänimen ja salasanan Base64-muodossa pyynnön otsikossa. Vaikka se on helppo toteuttaa ja toimii useimmissa tapauksissa, se ei tarjoa kovin vahvaa suojaa, sillä Base64-muunnos voidaan purkaa helposti. Tästä syystä suositellaan käytettäväksi muita autentikointitapoja, kuten HTTPS ja OAuth.

## Katso myös
- [HTTP-pyynnöt ja autentikointi Swift-kirjaston avulla](https://swift.org/server-apis/#http-authentication-with-swift)
- [Peruskäyttäjäautentikointi - Wikipedia](https://fi.wikipedia.org/wiki/Perusk%C3%A4ytt%C3%A4j%C3%A4autentikointi)
- [HTTP-pyynnöt ja autentikointi - Dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)