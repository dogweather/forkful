---
title:                "Swift: Analysere html"
simple_title:         "Analysere html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har jobbet med webutvikling, har du sikkert vært borti HTML-koden til et nettsted. HTML står for "HyperText Markup Language" og er en kode som beskriver strukturen og innholdet på en nettside. Men hva om du ønsker å hente ut spesifikke data fra en nettside, som for eksempel nyhetsoverskrifter eller produktbeskrivelser? Da trenger du å parse, eller analysere og hente ut dataene fra HTML-koden. Heldigvis er det enkelt å gjøre dette ved hjelp av Swift-programmering.

## Hvordan
Først må du importere "Foundation" og "UIKit" modulene i Swift. Deretter kan du bruke metoden "dataTask" fra "URLSession" for å hente innholdet på en nettside som en "Data" objekt. Dette gjøres ved å opprette en "URL" objekt med nettadressen til nettsiden du ønsker å parse. Deretter bruker du "dataTask" metoden til å hente innholdet og lagre dette i en variabel. Fra denne "Data" variabelen kan du bruke metoden "string" for å konvertere det til en lesbar tekst. 

For å parse HTML-koden, trenger du et parserbibliotek som "Kanna" eller "SwiftSoup". Disse bibliotekene gjør det enkelt å navigere i HTML-koden og hente ut spesifikke elementer ved hjelp av CSS-selektorer. For eksempel kan du bruke "select" metoden i "SwiftSoup" for å hente alle overskriftene på en nettside og lagre dem i en liste. Deretter kan du enkelt skrive ut overskriftene ved å bruke en "for"-løkke.

<code>Swift 
let url = URL(string: "https://www.example.com")
let task = URLSession.shared.dataTask(with: url!) {(data, response, error) in
    let htmlContent = String(data: data!, encoding: .utf8)
    let doc: Document = try! SwiftSoup.parse(htmlContent!)
    let headlines = try! doc.select("h2").map { try! $0.text() }
    for title in headlines {
        print(title)
    }
}
task.resume()
</code>

Dette eksempelet henter alle overskriftene på nettsiden "example.com" og skriver dem ut til konsollen. Ved å bruke forskjellige CSS-selektorer eller metoder, kan du hente ut ulike typer data fra HTML-koden.

## Dypdykk
Når du skal parse HTML, er det viktig å ha en god forståelse for kodingen og syntaksen til HTML. Dette gjør det enklere å navigere og hente ut dataene du ønsker. Det er også viktig å være bevisst på at HTML-koden på ulike nettsider kan være forskjellig, så du må kanskje justere CSS-selektorene eller metoden din for å få de ønskede resultaten.

I tillegg er det viktig å velge et pålitelig og godt parserbibliotek, da det kan være vanskelig å håndtere ulike typer HTML-kode på egenhånd. Forskning på ulike biblioteker og deres funksjonalitet kan være nyttig for å finne det som passer best for ditt prosjekt.

## Se også
- [Kanna](https://github.com/tid-kijyun/Kanna)
- [SwiftSoup](https://github.com/scinfu/SwiftSoup)
- [HTML for dummies](https://www.dummies.com/programming/html/)
- [W3Schools HTML Tutorial](https://www.w3schools.com/html/)