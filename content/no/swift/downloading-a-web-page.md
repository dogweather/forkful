---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside handler om å hente data fra internett så vi kan jobbe med det lokalt. Dette er nyttig når programmerere bygger funksjoner som skraper data fra websider, eller når det er nødvendig å lagre nettinnhold for offline tilgang.

## Hvordan gjøre det:

Her er en enkel måte å laste ned nettsider med Swift ved hjelp av klassen `URLSession`.

```Swift
import Foundation

let url = URL(string: "http://...") // sett inn din egen URL her
let task = URLSession.shared.dataTask(with: url!) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Data received:\n\(str ?? "")")
    }
}
task.resume()
```

Når du kjører denne koden, vil output være innholdet fra den oppgitte nettsiden.

## Dypdykk 

Historisk sett, hadde man tidligere påkrevde tredjeparts biblioteker for å laste ned nettsider, men nåværende versjoner av Swift har innebygd support for dette, som vist i eksempelet over. 

For alternativer, kan du se på Alamofire, som er en populær tredjeparts nettverksbibliotek for Swift. Det tilbyr mer omfattende funksjonalitet, inkludert bedre feilhåndtering og mer.

Når det gjelder implementeringsdetaljer, er det viktig å merke seg at koden ovenfor kjører asynkront. Det betyr at programmet vil fortsette å kjøre andre oppgaver på samme tid, og det vil ikke vente på at nettverksinnkallingen skal fullføres før det fortsetter.

## Se Også 

Hvis du ønsker å lære mer, her er noen ekstra ressurser:

1. Apple Developer Documentation om URLSession: [Her](https://developer.apple.com/documentation/foundation/urlsession)
2. Alamofire Home Page: [Her](https://github.com/Alamofire/Alamofire)