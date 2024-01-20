---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP-pyynnön lähettäminen Swift-ohjelmointikielellä

## Mitä ja Miksi?

HTTP-pyyntö on tapa, jolla ohjelmisto antaa tietoa tai pyytää sitä verkkopalvelimelta. Tämä taito on tärkeä, koska se mahdollistaa tietojen vaihtamisen ohjelmasi ja verkon välillä.

## Miten tehdään:

Aloitetaan perus HTTP GET -pyynnöllä Swiftissä. Tässä esimerkissä fetchataan dataa jostakin URL-osoitteesta.

```Swift
import Foundation

let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```

Tämä koodi tulostaa palvelimelta saadun datan.


## Syvemmälle:

HTTP-pyynnöt ovat olleet olemassa 90-luvun alkupuolelta lähtien, ja niitä käytetään yhä. Swiftissä on useita tapoja lähettää HTTP-pyyntöjä. Yllä esitetty URLSession on yksi yleisimmistä, mutta vaihtoehtoisesti voitaisiin käyttää myös esimerkiksi Alamofire-kirjastoa, joka tarjoaa hieman enemmän mukavuuksia ja ominaisuuksia.

Kun lähetämme pyynnön URLSessionin kautta, se luo tehtävän (task), joka suorittaa pyynnön. Koska verkko-operaatiot voivat kestää aikaa, tämä toteutus suoritetaan taustalla, jottei se pysäyttäisi käyttöliittymän päivitystä. Tulos palaa sitten takaisin pääsäikeeseen.

## Katso myös:

- [Alamofire kirjasto](https://github.com/Alamofire/Alamofire)
- [Apple dokumentaatio URLSessionista](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP-tietoja](https://developer.mozilla.org/fi/docs/Web/HTTP)