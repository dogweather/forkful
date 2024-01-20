---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTML-jäsentäminen on prosessi, jossa HTML-teksti muutetaan rakenteelliseksi näkymäksi, jota ohjelmat ja skriptit voivat ymmärtää. Tekijät hyödyntävät sitä informaation ja datan skrapaamiseksi verkkosivustoilta tai HTML-pohjaisten tiedostojen käsittelemiseksi.

## Kuinka?
Voimme käyttää Swift pakettia nimeltä `SwiftSoup` helposti jäsentämään HTML-tekstin Swiftillä. Ensin sinun tulee asentaa paketti CocoaPods avulla seuraavalla koodilla:

```Swift
pod 'SwiftSoup'
```

Tämän jälkeen voimme käyttää `SwiftSoup` jäsentääksemme HTML-tiedostoja. Tässä esimerkki:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Sivunimi</title></head><body>Tervetuloa!</body></html>"
    let doc: Document = try SwiftSoup.parse(html)
    let title: Element =  try doc.select("title").first()!
    print(try title.text())
} catch Exception.Error(_, let message) {
    print(message)
} catch {
    print("catch-all")
}
```

Kun ajat tämän koodin, näet tulostuksen:

```
Sivunimi
```

## Syväsukellus
HTML-jäsentämisen historia ulottuu aina 1990-luvun alkuvuosille, jolloin kehitettiin ensimmäisiä WWW-selaimia. Vaihtoehtoisia metodeja HTML:n jäsentämiselle ovat esimerkiksi jäsentimen itse kirjoittaminen tai valmiiden kirjastojen (kuten SwiftSoupin) käyttäminen. On kuitenkin tärkeää huomata, että HTML-jäsentämistä ei aina kannata tehdä itse, sillä se voi olla aikaa vievää ja monimutkaista. Lisäksi, pakettien kuten `SwiftSoup` avulla voi saada hyödyllisiä toiminnallisuuksia, kuten virheidenkäsittelyn ja tehokkaan suorituskyvyn.

## Katso Myös
1. SwiftSoup GitHub repo: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
3. HTML-jäsentämisen historia ja tekniikat: [https://stackoverflow.com/questions/432992/technique-and-tools-for-parsing-html-documents](https://stackoverflow.com/questions/432992/technique-and-tools-for-parsing-html-documents)