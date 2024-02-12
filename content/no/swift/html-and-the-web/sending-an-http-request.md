---
title:                "Å sende en HTTP-forespørsel"
aliases:
- /no/swift/sending-an-http-request.md
date:                  2024-01-20T18:00:57.433142-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel innebærer å be om data fra, eller sende data til en server over internett. Programmerere gjør dette for å hente eller sende informasjon til netttjenester og API-er for å bygge interaktive og nettverksbaserte applikasjoner.

## Hvordan:
```Swift
import Foundation

// Definer URL-en vi skal sende forespørselen til
if let url = URL(string: "https://api.example.com/data") {
    // Opprett en URLSession
    let session = URLSession.shared

    // Opprett en URLRequest med URL
    var request = URLRequest(url: url)
    request.httpMethod = "GET" // Metoden for forespørselen, GET henter data

    // Send forespørselen og håndter data eller feil
    let task = session.dataTask(with: request) { data, response, error in
        // Sjekk for feil
        if let error = error {
            print("Det oppstod en feil: \(error)")
            return
        }

        // Sjekk respons og pakk ut data hvis tilgjengelig
        if let httpResponse = response as? HTTPURLResponse,
           httpResponse.statusCode == 200,
           let jsonData = data {
            // Behandle den mottatte dataen
            print("Data mottatt: \(jsonData)")
        } else {
            print("Uh oh! Noe gikk galt med responsen.")
        }
    }

    // Start forespørselen
    task.resume()
}
```
Eksempelutdata: `Data mottatt: 101 bytes`

## Fordypning
Før i tiden, spesielt i Web 1.0s dager, brukte man ofte enklere, statiske forespørsler. Nå er det moderne applikasjoner med RESTful API-er og dynamisk innhold som regjerer. Istedenfor gamle skoler som CGI-script, brukes nå rammeverk som Express.js og SwiftUI for å håndtere HTTP-forespørsler. Alternativer til Swifts `URLSession` kunne være biblioteker som Alamofire for mer komplekse behov. Viktige detaljer ved å sende en HTTP-forespørsel inkluderer sikkert håndtering av data, parsing av JSON, og forståelse av HTTP statuskoder.

## Se Også
- [Swifts URLSession dokumentasjon](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP statuskoder forklart](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Alamofire GitHub prosjekt](https://github.com/Alamofire/Alamofire)
