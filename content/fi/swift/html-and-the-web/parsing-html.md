---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:30.800522-07:00
description: "HTML:n j\xE4sent\xE4minen viittaa HTML-sis\xE4ll\xF6n rakenteen purkamiseen\
  \ ja tulkitsemiseen, tyypillisesti tiettyjen tietojen poimimiseksi tai t\xE4m\xE4\
  n sis\xE4ll\xF6n\u2026"
lastmod: '2024-02-25T18:49:53.813434-07:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen viittaa HTML-sis\xE4ll\xF6n rakenteen purkamiseen\
  \ ja tulkitsemiseen, tyypillisesti tiettyjen tietojen poimimiseksi tai t\xE4m\xE4\
  n sis\xE4ll\xF6n\u2026"
title: "HTML:n j\xE4sennys"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen viittaa HTML-sisällön rakenteen purkamiseen ja tulkitsemiseen, tyypillisesti tiettyjen tietojen poimimiseksi tai tämän sisällön ohjelmallisesti käsittelyyn. Ohjelmoijat osallistuvat HTML:n jäsentämiseen verkon kaapimiseen, tiedon louhintaan, automatisoituun testaukseen ja sisällön migraatioon, mahdollistaen sovellusten tehokkaan vuorovaikutuksen ja prosessoinnin web-dokumenttien kanssa.

## Kuinka:
Swift ei oletusarvoisesti sisällä sisäänrakennettua kirjastoa HTML:n jäsentämiseen, mikä edellyttää kolmannen osapuolen kirjastojen käyttöä tämän tehtävän tehokkaaseen käsittelyyn. Yksi suosituimmista valinnoista on SwiftSoup, puhdas Swift-kirjasto, joka tarjoaa jQueryn kaltaisen syntaksin HTML:n jäsentämiseen ja manipulointiin.

### Asennus
Ensiksi, sinun on lisättävä SwiftSoup projektiisi. Jos käytät Swift Package Manageria, voit lisätä sen `Package.swift` riippuvuuksiisi:

```swift
riippuvuudet: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", alkaen: "2.3.2")
]
```

### Esimerkki: Linkkien poimiminen HTML:stä
Oletetaan, että sinulla on HTML-dokumentti ja haluat poimia kaikki linkit (`<a href="...">`). SwiftSoupin avulla voit saavuttaa tämän helposti:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>Esimerkkisivu</title>
</head>
<body>
    <p>Tervetuloa verkkosivustollemme</p>
    <a href="https://example.com/page1">Sivu 1</a>
    <a href="https://example.com/page2">Sivu 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let tyyppi, let viesti) {
    print("Virheen tyyppi: \(tyyppi) Viesti: \(viesti)")
} catch {
    print("virhe")
}
```

### Esimerkkituloste
Edellinen koodi poimii URL-osoitteet ja niiden tekstit HTML:stä, tulostaen:

```
Sivu 1 - https://example.com/page1
Sivu 2 - https://example.com/page2
```

Tämä perusesimerkki osoittaa, miten hyödyntää SwiftSoupia HTML-dokumenttien jäsentämiseen. Tutustumalla lisää SwiftSoupin dokumentaatioon, voit löytää lukuisia menetelmiä selata, etsiä ja muokata HTML-sisältöä, valtuuttaen Swift-sovelluksesi käsittelemään monimutkaista web-sisältöä vaivattomasti.
