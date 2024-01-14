---
title:                "Swift: Et Sendt http-forespørsel"
simple_title:         "Et Sendt http-forespørsel"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Bruken av HTTP requests er et viktig konsept i Swift-programmering, spesielt når man ønsker å kommunisere med eksterne servere eller nettjenester. Ved å sende en HTTP request, kan man enkelt få tak i informasjon fra en ekstern kilde og bruke den i sin egen kode.

## Hvordan

For å sende en HTTP request i Swift, trenger vi først å importere `Foundation` biblioteket. Deretter kan vi bruke `URLSession` klassen til å opprette en `URLRequest` som inneholder den ønskede URL-en. Nedenfor finner du et eksempel på hvordan dette kan gjøres:

```Swift
import Foundation

// Opprett en URL
let url = URL(string: "https://api.website.com/data")

// Opprett en URLRequest
let request = URLRequest(url: url!)

// Opprett en URLSession
let session = URLSession.shared

// Utfør HTTP request
let task = session.dataTask(with: request) { data, response, error in
    // Behandle data / feil
    if let error = error {
        print("Feil ved å sende HTTP request: \(error)")
        return
    }
    
    // Tilgang til responsen og behandle dataene
    if let response = response {
        print("HTTP responskode: \(response.statusCode)")
    }
    
    if let data = data {
        print("Data fra responsen: \(data)")
    }
}

task.resume()
```

I dette eksempelet opprettes det først en `URL` og deretter en `URLRequest` med denne URL-en. Deretter opprettes en `URLSession` og en `dataTask` for å utføre selve HTTP requesten. Innenfor `dataTask`-blokken har vi tilgang til både responsen og eventuelle data som blir returnert. Dette kan være json-data, bilder eller annen informasjon, avhengig av hva den eksterne kilden returnerer.

I eksempelet over blir responsen og dataene kun printet ut til konsollen, men i ditt eget prosjekt kan du behandle dataene på den måten som passer best.

## Dypdykk

Når vi sender en HTTP request, bruker vi et bestemt HTTP-verb for å spesifisere hva slags handling vi vil utføre. De vanligste verbene er `GET`, `POST`, `PUT` og `DELETE`. Disse spesifiserer henholdsvis å hente data, legge til nye data, oppdatere eksisterende data og slette data.

I tillegg til å sende enkle HTTP requests, kan vi også sende mer komplekse requests med ulike parametere og headers. Dette kan være nyttig for å autentisere brukere og sikre sikker kommunikasjon med eksterne kilder.

## Se Også

- [Apple dokumentasjon om URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Artikkel om HTTP requests i Swift fra Hacking with Swift](https://www.hackingwithswift.com/read/39/2/how-to-read-json-from-an-http-request)
- [Eksempelprosjekt på GitHub om HTTP request i Swift](https://github.com/typicaljoe/Parse.com-Swift)