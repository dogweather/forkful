---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering er en metode for å oppnå kommunikasjon mellom klientsidig programvare og en server ved å dele legitimasjon. Utviklere gjør dette for å sikre at bare autoriserte brukere har tilgang til visse serverressurser.  

## Hvordan:

Her er et Swift eksempel på hvordan man sender en HTTP-forespørsel med grunnleggende autentisering:

``` Swift
import Foundation

let url = URL(string: "https://example.com")!
let username = "your-username"
let password = "your-password"
let loginString = String(format: "%@:%@", username, password)

let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```

Når du kjører denne koden i en Playground, vil du se utdata som ser noe ut som dette:

``` Swift
Received data:
{
    "active": true,
    "locked": false,
    "name": "Your name",
    "roles": [
        "User"
    ]
}
```

## Dypdykk

Historisk sett innførte RFC 7617 grunnleggende autentiseringsregimet for HTTP som vi kjenner i dag. Til tross for sin enkelhet, er det viktig å merke seg at grunnleggende autentisering overfører legitimasjon i klartekst (bare base64-kodet) over nettverket, så det brukes best sammen med HTTPS for å beskytte integriteten til brukerens opplysninger.

Alternativt kan du bruke mer sikre metoder for autentisering mens du arbeider med HTTP-forespørsler, som OAuth 2.0 og JSON Web Tokens (JWT), avhengig av spesifikke krav og bruksscenarier.

Når det gjelder implementeringsdetaljer i Swift, bruker vi `URLSession`-biblioteket for å lage forespørsler. Du oppretter en `URLRequest` for å sette opp detaljene i HTTP-forespørselen, inkludert metoden (GET, POST, etc.), URL-en til serveren vil du koble til og `Authorization` headeren med base64-enkodede opplysninger.

## Se Også

- Om grunnleggende autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme
- Swift-nettverkspraksis: https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started
- Alternativer til grunnleggende autentisering: https://auth0.com/blog/cookies-vs-tokens-definitive-guide/