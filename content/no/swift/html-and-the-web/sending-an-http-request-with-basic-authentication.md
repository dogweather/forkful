---
date: 2024-01-20 18:02:52.083730-07:00
description: "Hvordan: F\xF8r HTTPS og mer komplekse autentiseringsmetoder ble standard,\
  \ ble grunnleggende autentisering mye brukt for \xE5 verifisere brukere over internett\u2026"
lastmod: '2024-04-05T22:50:55.148466-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r HTTPS og mer komplekse autentiseringsmetoder ble standard, ble grunnleggende\
  \ autentisering mye brukt for \xE5 verifisere brukere over internett ved hjelp av\
  \ Base64-koding for \xE5 sende brukernavn og passord."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan:
```Swift
import Foundation

// Opprett din URL
if let url = URL(string: "https://eksempel.no/api/data") {

    // Forbered en URLRequest
    var request = URLRequest(url: url)
    request.httpMethod = "GET"

    // Legg til grunnleggende autentiseringsinformasjon
    let brukernavn = "dittBrukernavn"
    let passord = "dittPassord"
    let loginString = "\(brukernavn):\(passord)"
    if let loginData = loginString.data(using: .utf8) {
        let base64LoginString = loginData.base64EncodedString()
        request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
    }

    // Send forespørselen
    let session = URLSession.shared
    let task = session.dataTask(with: request) { data, response, error in
        guard let data = data, error == nil else {
            print("Feil ved forespørsel: \(error?.localizedDescription ?? "Ukjent feil")")
            return
        }
        if let responseString = String(data: data, encoding: .utf8) {
            print("Respons:\n\(responseString)")
        }
    }
    task.resume()
}
```
Sample output:
```
Respons:
{"eksempelData": "Verdi"}
```

## Dypdykk
Før HTTPS og mer komplekse autentiseringsmetoder ble standard, ble grunnleggende autentisering mye brukt for å verifisere brukere over internett ved hjelp av Base64-koding for å sende brukernavn og passord. Riktignok er det ikke så sikkert som moderne metoder fordi Base64 er enkelt å dekode, så i dag brukes det oftere på interne nettverk eller over HTTPS. Alternative autentiseringsmetoder inkluderer OAuth og API-nøkler, som er sikrere valg for eksponerte APIer. Implementasjonsdetaljer for grunnleggende autentisering inkluderer koding av brukernavn og passord, konstruksjon av en passende HTTP-header, og håndtering av responsen.

## Se Også
- [URLRequest - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlrequest)
- [URLSession - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP authentication - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basic access authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
