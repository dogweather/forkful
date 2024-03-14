---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:49.408510-07:00
description: "HTML parsen betekent het filteren door de soep van code van een website\
  \ om nuttige brokjes te vinden \u2014 tekst, links, afbeeldingen, etc. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-13T22:44:51.154594-06:00'
model: gpt-4-0125-preview
summary: "HTML parsen betekent het filteren door de soep van code van een website\
  \ om nuttige brokjes te vinden \u2014 tekst, links, afbeeldingen, etc. Programmeurs\
  \ doen\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen betekent het filteren door de soep van code van een website om nuttige brokjes te vinden — tekst, links, afbeeldingen, etc. Programmeurs doen dit om gegevens te extraheren, webinteracties te automatiseren of inhoud in hun apps te importeren.

## Hoe:

Swift heeft geen ingebouwde HTML-parsing; we hebben een hulpje nodig. Laten we SwiftSoup gebruiken, een Swift-library die doet denken aan Python's BeautifulSoup. Voeg eerst SwiftSoup toe aan je project met behulp van Swift Package Manager.

Zo doe je dat:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>Eerste parse</title></head>"
                + "<body><p>HTML geparsed in een doc.</p></body></html>"
    let doc = try SwiftSoup.parse(html)
    let title = try doc.title()
    let bodyText = try doc.body()?.text()
    
    print(title) // Output: Eerste parse
    print(bodyText) // Output: HTML geparsed in een doc.
} catch Exception.Error(let type, let message) {
    print("Er is een fout van het type: \(type) opgetreden: \(message)")
} catch {
    print("Er is een onbekende fout opgetreden")
}
```

## Diepteanalyse

HTML of HyperText Markup Language is sinds Tim Berners-Lee het (en het web) in 1991 introduceerde de ruggengraat van het web. Naarmate het web evolueerde, deed HTML dat ook, wat de parsing complexiteit verhoogde.

Hier is waarom SwiftSoup uitblinkt:
- **Gebruikersvriendelijk**: De API lijkt op JQuery, wat betekent dat het intuïtief is voor degenen die bekend zijn met webontwikkeling.
- **Robuustheid**: Gaat goed om met de eigenaardigheden van real-world HTML.
- **Performance**: Swift is snel, wat belangrijk is voor grote parsingtaken.

Alternatieven? Zeker!
- **WebKit**: Gebruik dit voor zwaardere taken zoals het renderen van webpagina's of het uitvoeren van JavaScript.
- **libxml2**: Hardcore C-route, maar je moet er klaar voor zijn.
- **Regex**: Gewoon nee. Het is geen parser. Probeer niet om HTML te “parsen” met regex. Echt niet.

Onthoud echter dat een parser zoals SwiftSoup de pagina niet gewoon leest zoals deze is; het is zich niet bewust van inhoud die dynamisch wordt geladen door JavaScript. Voor dat, ga naar oplossingen met WebKit of browser headless modi.

## Zie Ook

- SwiftSoup op GitHub: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift Package Manager: [https://swift.org/package-manager/](https://swift.org/package-manager/)
- WebKit-documentatie: [https://developer.apple.com/documentation/webkit](https://developer.apple.com/documentation/webkit)
- Omgaan met dynamische inhoud: [Selenium WebDriver](https://www.selenium.dev/documentation/en/) (niet specifiek voor Swift maar relevant voor geautomatiseerde interacties met dynamische webpagina's)
