---
title:                "Swift: Lähetettävä http-pyyntö perustuvalla todennuksella."
simple_title:         "Lähetettävä http-pyyntö perustuvalla todennuksella."
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettäminen perusautentikoinnilla on tärkeä osa web-kehitystä ja mahdollistaa turvallisen kommunikoinnin palvelimien ja asiakasohjelmien välillä.

## Kuinka

```Swift
let username = "käyttäjänimi"
let password = "salasana"

let credentials = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)
if let base64Encoded = credentials?.base64EncodedString(options: NSData.Base64EncodingOptions(rawValue: 0)) {
    let urlString = "https://esimerkki.com/api"
    let url = URL(string: urlString)!
    var request = URLRequest(url: url)
    request.addValue("Basic \(base64Encoded)", forHTTPHeaderField: "Authorization")
    URLSession.shared.dataTask(with: request) { data, response, error in
        if let data = data {
            print(data)
        }
    }.resume()
}
```

Koodiesimerkissä näytetään, kuinka lähettää HTTP-pyyntö perusautentikoinnilla käyttäen Swiftin Foundation-kirjaston URLSession-oliota. Pyyntöön lisätään tarvittava autentikointitieto "Authorization" -headeriksi.

## Syvällinen tarkastelu

HTTP-pyynnöissä käytetty perusautentikointi perustuu käyttäjänimen ja salasanan lähettämiseen encodattuna Base64-muodossa. Tämä ei kuitenkaan ole täysin turvallinen tapa autentikoida, sillä Base64-muunnettu tieto on helppo purkaa takaisin alkuperäiseen muotoon.

On suositeltavaa käyttää vahvempia autentikointimenetelmiä, kuten OAuth, jos mahdollista.

## Katso myös

[Foundation Framework - Apple Developer Documentation](https://developer.apple.com/documentation/foundation)  
[URLSession - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)  
[Basic access authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)