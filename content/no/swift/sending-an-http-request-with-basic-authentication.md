---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Swift: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor sender vi HTTP-forespørsler med grunnleggende autentisering? Grunnene kan være mange, men en av de viktigste er å sikre at våre nettsteder og applikasjoner bare lar autoriserte brukere få tilgang til sensitive data og funksjoner.

## Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i Swift, må du først importere "Foundation" -rammen. Deretter kan du sette opp forespørselen ved å definere en URL og legge til autentiseringsinformasjon i en "Authorization" -header.

```Swift
import Foundation

// Definer URL-en
let url = URL(string: "http://nettsted.com/api/data")

// Sett opp request
var request = URLRequest(url: url!)

// Definer autentisering
let username = "brukernavn"
let password = "passord"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()

// Legg til autentisering i headeren
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Eksekver forespørselen og håndter svaret
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Feil: \(error)")
    } else if let responseData = data {
        // Behandle svaret her
    }
}

task.resume()
```

Et eksempel på svaret kan være en streng som inneholder JSON-data. Vi kan bruke Swifts "JSONSerialization" til å dekode denne dataen og få tilgang til den.

```Swift
if let json = try? JSONSerialization.jsonObject(with: responseData, options: []) {
    // Dataen er nå et Swift-objekt, og vi kan behandle den videre
    print(json)
}
```

## Dypdykk

Grunnleggende autentisering er en enkel og vanlig måte å sikre HTTP-forespørsler på. Den fungerer ved å kodere brukernavn og passord og sende dem som en del av forespørselen. På serverens side dekoder den disse verdiene og sjekker om de stemmer overens med autoriserte brukerkontoer. Det er viktig å merke seg at denne metoden ikke krypterer dataen som sendes, så den bør brukes sammen med en sikker tilkobling (HTTPS).

## Se også

- [Swift.org](https://swift.org/)
- [Foundation-rammen](https://developer.apple.com/documentation/foundation)
- [URLSession](https://developer.apple.com/documentation/foundation/urlsession)