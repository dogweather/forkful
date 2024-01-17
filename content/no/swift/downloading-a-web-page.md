---
title:                "Nedlasting av en nettside"
html_title:           "Swift: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Nedlasting av en nettside er prosessen med å få tilgang til og hente innhold fra en nettside ved hjelp av en datamaskin eller annen enhet tilkoblet internett. Programmere gjør dette for å kunne behandle eller manipulere data fra nettsiden, for eksempel for å lage en app eller for å lage en automatisering.

## Slik gjør du det:

```Swift
// Opprette en URL-objekt for å representere nettadressen
let url = URL(string: "https://www.example.com")

// Opprette en URLRequest for å gjøre en forespørsel til nettadressen
if let request = URLRequest(url: url) {
    // Opprette en URLSession for å håndtere nedlastingsoppgaven
    let session = URLSession(configuration: .default)

    // Opprette en dataTask basert på forespørselen og håndtere resultatet
    let dataTask = session.dataTask(with: request) { (data, response, error) in
        // Sjekke om det var en feil og behandle den hvis det er tilfelle
        if let error = error {
            print("Det oppstod en feil: \(error)")
        }
        
        // Håndtere responsen og konvertere dataen til en streng for enkel bruk
        if let data = data, let httpResponse = response as? HTTPURLResponse {
            print("Forespørselen var vellykket med status \(httpResponse.statusCode)")
            if let html = String(data: data, encoding: .utf8) {
                print("Innholdet på nettsiden er: \(html)")
            }
        }
    }
    
    // Start nedlastingen ved å kalle dataTask
    dataTask.resume()
}
```

## Dykke dypere:

Det å kunne programmere nedlasting av nettsider har vært en viktig del av utviklingen av moderne internett. Dette har blitt gjort på ulike måter, som for eksempel ved bruk av protokoller som HTTP og FTP. Det finnes også alternative måter å gjøre dette på, som for eksempel ved bruk av tredjepartsbiblioteker som Alamofire eller forhåndsinstallerte funksjoner som URLSession i Swift. Implementeringen av nedlasting av en nettside kan også involvere DNS-forespørsler og håndtering av sesjoner.

## Se også:

- [Apple Developer Documentation for URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Open-source HTTP-biblioteket Alamofire](https://github.com/Alamofire/Alamofire)
- [W3Schools tutorial om HTTP-protokollen](https://www.w3schools.com/tags/ref_httpmethods.asp)