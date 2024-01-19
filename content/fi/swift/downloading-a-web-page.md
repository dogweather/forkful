---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## “Mikä & Miksi?”

Web-sivun lataaminen tarkoittaa tietojen hankkimista verkkosivulta ohjelmasi käyttöön. Ohjelmoijat tekevät tämän esimerkiksi verkkosivulta tulevan tiedon hyödyntämiseksi ohjelman logiikassa.

## “Miten se tehdään:”

Seuraavassa on esimerkki siitä, kuinka ladataan verkkosivu Swiftissä:

```Swift
import Foundation

let url = URL(string: "https://www.example.com")
let task = URLSession.shared.dataTask(with: url!) {(data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print(str) // Konsoliin tulostuu esimerkkisivun HTML-koodi
    }
}
task.resume()
```

## “Syvemmälle:”

Web-sivun lataamisen ohjelmoinnin alkuaikojen taustalla on niinkin yksinkertainen asia kuin tietojen hankkiminen suoraan internetistä. Nykyään on olemassa erilaisia keinoja ja tekniikoita, kuten "BeautifulSoup" Pythonissa tai "jsoup" Javassa.

Swiftin `URLSession.shared.dataTask(with:completionHandler:)` on kätevin tapa ladata tietoja. Se hoitaa monimutkaiset yksityiskohdat, kuten verkkoyhteyksien hallinta ja multithreading. 

Huomaa kuitenkin, että tämä luo uuden tietojenkäsittelytehtävän ja käynnistää tehtävän (`task.resume()`). Ilman `task.resume()`, tehtävää ei koskaan suoriteta.

## “Katso myös:”

Lisäreferenssejä Swiftin verkkosivun lataamisesta ja sen toiminnallisuudesta:

1. [Apple Developer - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
2. [Swift 5 Network Programming](https://programmingwithswift.com/swift-networking-with-urlsession/)
3. [HackerRank - Downloading Web Content](https://www.hackerrank.com/topics/downloading-web-content-in-swift)