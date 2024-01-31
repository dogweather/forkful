---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:02:56.304409-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
HTTP-pyyntö perusautentikaatiolla on tapa lähettää käyttäjänimi ja salasana verkon yli turvallisesti. Ohjelmoijat käyttävät tätä tapaa, kun heidän täytyy varmistaa käyttäjän oikeudet palvelimiin tai API:hin pääsyssä.

## How to: (Kuinka tehdä:)
Swiftissä perusautentikaatio tapahtuu lisäämällä base64-koodattu "Authorization" header HTTP-pyyntöön. Tässä yksinkertainen esimerkki.

```Swift
import Foundation

// Käyttäjänimi ja salasana
let username = "kayttaja"
let password = "salasana"

// Luodaan autentikaatio-merkkijono
if let loginData = "\(username):\(password)".data(using: .utf8) {
    let base64LoginString = loginData.base64EncodedString()

    // Luodaan url ja url-pyyntö
    if let url = URL(string: "https://example.com/api/data") {
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

        // Lähetetään pyyntö
        let task = URLSession.shared.dataTask(with: request) { data, response, error in
            // Käsitellään vastaus
            guard let data = data, error == nil else {
                print(error?.localizedDescription ?? "Ei vastausta")
                return
            }

            if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode == 200 {
                // Onnistunut pyyntö
                if let responseString = String(data: data, encoding: .utf8) {
                    print("Vastaus: \(responseString)")
                }
            } else {
                // Virhekäsittely
                print("HTTP Status: \((response as? HTTPURLResponse)?.statusCode ?? 0)")
            }
        }
        task.resume()
    }
}
```

## Deep Dive (Syväsukellus)
Perusautentikaation käyttö HTTP-pyynnöissä juontaa juurensa webin alkuvaiheisiin. Se on yksinkertainen mekanismi, mutta ei turvallisin tapa lähettää arkaluonteisia tietoja, sillä base64-koodaus ei salaa tietoja. Moderni suositus käyttää HTTPS-protokollaa turvatakseen tietoja. Vaihtoehtoisia menetelmiä ovat OAuth ja API-avaimet, jotka tarjoavat lisää turvallisuutta.

Kun käytät perusautentikaatiota, tärkeää on muistaa:

1. Käytä aina HTTPS-yhteyttä, estääksesi tietojen kaappausta.
2. Säilytä käyttäjänimi ja salasana turvallisesti, esimerkiksi Keychainissa.
3. Ota huomioon palvelimen asettamat rajoitukset autentikaatiomerkkijonojen pituudelle.

## See Also (Lisätietoja)
- [Apple Developer Documentation: URL Loading System](https://developer.apple.com/documentation/foundation/url_loading_system)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [OWASP Guide to Authentication](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
