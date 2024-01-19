---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lähetämme HTTP-pyynnön perustodennuksella kun haluamme käyttäjätietojen turvaamisen webservissä istunnon aikana. Ohjelmoijat tekevät tämän, koska se on yleinen tapa suojata käyttäjän tietoja verkkopalveluissa.

## Kuinka:
Tässä on esimerkkikoodi, jolla lähetetään HTTP-pyyntö perustodennuksella Swiftissä. Siinä asetetaan käyttäjänimi ja salasana koodattuna `Authorization`-otsikkoon.

```Swift
import Foundation

let username = "kayttajatunnus"
let password = "salasana"

let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: URL(string: "https://www.esimerkki.fi")!)
request.httpMethod = "POST"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    // tässä käsitellään vastaus
}

task.resume()
```
## Syväsukellus
HTTP Basic Authentication on menetelmä, jossa käyttäjänimi ja salasana lähetetään HTTP-otsikoissa. Se on perinteisesti yksi yleisimmistä verkkotodennustapoja, mutta se ei ole turvallisin. Nykyään voidaan käyttää muitakin menetelmiä, kuten digitaalista allekirjoitusta tai OAuth-tokenien käyttöä.

Basic Authentication puutteellisuuksiin kuuluu, että jos verkkoistunto on salattu, käyttäjänimeä ja salasanaa ei lähetetä suojaamattomana tekstinä. Tämä tarkoittaa, että salauksen rikkoutuminen voi paljastaa käyttäjätietoja.

Jos tarvitset lisää suojautumista, harkitse jotain näistä vaihtoehdoista, mutta muista, että ne voivat olla enemmän aikaa vieviä toteuttaa.

## Katso myös
[ATP:n artikkeli Basic Authenticationista](https://linktoarticle)

[Stack Overflow: How to add basic authentication parameters in HTTP Header in Swift](https://stackoverflow.com/questions/24379601/how-to-add-basic-authentication-parameters-in-http-header-in-swift)

[Apple: URL Loading System in Swift](https://developer.apple.com/documentation/foundation/url_loading_system)