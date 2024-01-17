---
title:                "Html:n jäsentäminen"
html_title:           "Swift: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/parsing-html.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
HTML parsing tarkoittaa HTML-koodin lukemista ja sen sisältämien tietojen muokkaamista. Ohjelmoijat tekevät tätä usein tietojen esittämiseksi tai käsittelemiseksi, kuten hakukoneiden indeksoinnissa tai web-sovellusten kehittämisessä.

# Ohjeet:
Swift-koodia käyttämällä voit helposti parsia HTML-koodia ja käsitellä sen sisältämät tiedot. Tässä on esimerkki, joka hakee osoitteeseen https://www.google.com/piilota olevasta HTML-koodista Title-elementin sisällön ja tulostaa sen konsoliin:
```
Swift
let html = "https://www.google.com"
if let url = URL(string: html),
let data = try? Data(contentsOf: url),
let htmlString = String(data: data, encoding: .utf8),
let title = htmlString.slice(from: "<title>", to: "</title>") {
print(title)
}
```
Tulostus: Google

# Syvälle tarinassa:
HTML-parsing ei ole uusi käsite, sillä sen historia ulottuu Internetin alkuaikoihin. Alun perin HTML-parsing tehtiin manuaalisesti ja toivottiin suosiollista tulosta, mutta nykyään kehittyneet ohjelmistot tekevät sen automaattisesti. On myös olemassa muita ratkaisuja, kuten XML-parsing, mutta HTML on edelleen suosituin tekniikka web-sivujen sisällön muokkaamiseen.

# Katso myös:
- [Swiftin virallinen dokumentaatio HTML-parsingista](https://developer.apple.com/documentation/foundation/xmlparser)
- [HTML-parsingin merkitys hakukoneiden toiminnassa](https://www.searchenginejournal.com/html-parsing-seo/261735/)
- [HTML-parsingin mahdollisuudet web-sovellusten kehittämisessä](https://www.apriorit.com/dev-blog/403-swift-html-parsing-tutorial)