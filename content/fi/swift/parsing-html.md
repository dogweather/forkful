---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:33:55.582096-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? - "Mitä & Miksi?"
Parsing HTML on prosessi, missä HTML-koodi siirretään jäsennettävään muotoon. Tämä mahdollistaa sivuston sisällön hyödyntämisen ja käsittelyn ohjelmissa. 

## How to - "Kuinka tehdä:"
Swiftissä HTML:n jäsentämiseen on useita kirjastoja, mutta käydään läpi esimerkki käyttäen suosittua `SwiftSoup`-kirjastoa:

```Swift
import SwiftSoup

let htmlString = "<html><head><title>First parse</title></head>"
    + "<body><p>Parsed HTML into a doc.</p></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(htmlString)
    let title: String = try doc.title()
    let p: Element? = try doc.select("p").first()
    
    print(title) // Tulostetaan otsikko
    if let text = p?.text() {
        print(text) // Tulostetaan ensimmäinen kappale
    }
} catch Exception.Error(let type, let message) {
    print("Got an error of type \(type) with message \(message)")
} catch {
    print("error")
}
```

Tulostus:
```
First parse
Parsed HTML into a doc.
```

## Deep Dive - "Syväsukellus":
HTML:n jäsentämistä on tarvittu verkkosisällön lukemiseen alusta asti. Historiallisesti tämä on tehty käyttämällä Regular Expressions -säännöllisiä lausekkeita, mutta ne eivät ole ihanteellisia HTML:n monimutkaisuuden ja dynaamisuuden vuoksi.

SwiftSoup on moderni vaihtoehto, joka käsittelee HTML:n DOM-tyyliin, mikä on tuttua JavaScript-kehittäjille. Tämä tekee sen, että SwiftSoup osaa käsitellä myös huonosti muotoiltua HTML-koodia ja tarjoaa monia metodeja sisällön manipulointiin.

## See Also - "Katso myös":
- SwiftSoup GitHub -sivu: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift dokumentaatio: [https://swift.org/documentation/](https://swift.org/documentation/)
- DOM (Document Object Model) -tiedot: [https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)