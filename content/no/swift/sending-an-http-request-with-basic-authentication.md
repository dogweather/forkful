---
title:                "Swift: Sending et http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending et http-forespørsel med grunnleggende autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

HTTP-anmodninger med grunnleggende autentisering er en viktig del av mange programmeringsprosjekter. Det tillater brukere å sikre sine kommunikasjoner med servere ved å sende autentiseringsinformasjon i hvert anmodning.

# Hvordan

For å sende en HTTP-anmodning med grunnleggende autentisering i Swift, må du først legge til følgende importstatement i begynnelsen av filen din:

```Swift
import Foundation
```

Deretter kan du konstruere din HTTP-anmodning og legge til den nødvendige autentiseringsinformasjonen ved å bruke `URLCredential`-objektet.

```Swift
let urlString = "http://eksempel.com"
let url = URL(string: urlString)!
let username = "brukernavn"
let password = "passord"
let credentials = URLCredential(user: username, password: password, persistence: .forSession)
let session = URLSession(configuration: .default)
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("application/json", forHTTPHeaderField: "Content-Type")
request.setValue("application/json", forHTTPHeaderField: "Accept")
request.setValue("basic \(credentials)", forHTTPHeaderField: "Authorization")
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("An error occurred: \(error.localizedDescription)")
    }
    if let httpResponse = response as? HTTPURLResponse {
        print("Status code: \(httpResponse.statusCode)")
    }
    if let responseData = data {
        do {
            let json = try JSONSerialization.jsonObject(with: responseData, options: [])
            print("Response data: \(json)")
        } catch {
            print("Could not serialize response data.")
        }
    }
}
task.resume()
```

# Dypdykk

Det er viktig å merke seg at HTTP-anmodninger med grunnleggende autentisering er mindre sikre enn andre autentiseringsteknikker, da brukernavn og passord sendes i klartekst. Det anbefales derfor å bruke HTTPS når du implementerer grunnleggende autentisering. I tillegg tillater denne metoden kun én bruker per anmodning, noe som kan være en begrensning i visse scenarier.

# Se også

- [HTTP authentication with Swift](https://mitchellb.github.io/2015/06/18/http-basic-authentication-with-swift.html)
- [URLCredentialClass Reference](https://developer.apple.com/documentation/foundation/urlcredential)
- [URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)