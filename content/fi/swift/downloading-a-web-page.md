---
title:                "Verkkosivun lataaminen"
html_title:           "Swift: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi
Yksi yleinen syy ladata verkkosivu on tietojen kerääminen jälkikäteen, tai mahdollisuus tarkastella sivuston sisältöä offline-tilassa.

## Kuinka
Asettamalla URL-osoite ja käyttämällä URLSession-luokan metodia, pystyt lataamaan web-sivun helposti.

```Swift
// Esimerkki koodi
let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
   guard let data = data else { return }
   // Käsittele ladattu data tässä
}
task.resume()
```

Saat ladatun datan käyttämällä URL-tietojen latauspyynnön sisältämää "data" parametria. Tämän jälkeen voit käsitellä dataa haluamallasi tavalla.

```
// Esimerkki tulostus
print(data)
```

## Syventävä sukellus
Lisäksi voit määrittää latauspyyntöön lisäparametreja, kuten timeout-ajan ja datan käsittelemistavan. Voit myös käsitellä ladattua datatyyppiä, kuten kuvia tai JSON-muotoista dataa, eri tavoin.

## Katso myös
- Apple:n virallinen dokumentaatio URLSessionista: https://developer.apple.com/documentation/foundation/urlsession
- Kattava opas URL-näyttämiseen Swiftissä: https://www.raywenderlich.com/5895-urlsession-tutorial-getting-started