---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML betyr å lese og analysere en HTML-kode. Programmerere gjør det for å hente ut og forstå informasjonen på en web-side eller i et dokument.

## Hvordan:

Her viser jeg hvordan du kan bruke `SwiftSoup`, et kraftig Swift-bibliotek, for å parse HTML.

Først, legg til SwiftSoup til ditt projekt ved å bruke Swift Package Manager. Legg til dette i din `Package.swift`:

```Swift
.package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.0.0")
```

Nå, kan vi begynne å parse HTML. Vi begynner med en enkel HTML streng.

```Swift
import SwiftSoup

let html = "<html><head><title>Mitt nettsted</title></head><body>Hei verden!</body></html>"

do {
    let doc: Document = try SwiftSoup.parse(html)
    let title: Element = try doc.select("title").first()!
    print(try title.text())
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("unknown error")
}
```

Dette programmet vil skrive ut "Mitt nettsted", tittelen på vår HTML-streng.

## Dypdykk

HTML parsing har en lang historie dating tilbake til begynnelsen av web-utvikling. Parser-biblioteker som `BeautifulSoup` i Python og `Jsoup` i Java har lenge vært populære. `SwiftSoup` er basert på `Jsoup`, og gir samme funksjoner i Swift.

Alternativer til `SwiftSoup` inkuderer `Kanna` og `Swift-HTML-Parser`, men `SwiftSoup` tilbyr en mer robust funksjonalitet og bedre feilhåndtering.

Når det kommer til implementasjonsdetaljer, bygger `SwiftSoup` opp en DOM (Document Object Model) fra HTML-teksten. DOM er en datastruktur som lar programmereren manipulere elementer på en side som om de var objekter i et program. 

## Se Også

For mer informasjon og eksempler på parsing med SwiftSoup, sjekk ut [SwiftSoup GitHub- prosjektet](https://github.com/scinfu/SwiftSoup). For dokumentasjon på DOM, se [Mozilla's dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model).