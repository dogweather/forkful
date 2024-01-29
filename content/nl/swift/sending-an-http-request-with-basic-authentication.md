---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:08:01.376587-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden met basisauthenticatie houdt in dat een gebruikersnaam en wachtwoord aan een verzoek voor afgeschermde webinhoud worden toegevoegd. Programmeurs doen dit om toegang te krijgen tot API's of bronnen die beperkt zijn tot geautoriseerde gebruikers.

## Hoe:

Zo verstuur je een HTTP-verzoek met basisauthenticatie in Swift:

```Swift
import Foundation

// Je API-eindpunt
let url = URL(string: "https://example.com/api/data")!

// Je inloggegevens
let username = "user"
let password = "password"

// Maak inloggegevens en converteer naar base64-string
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// Maak het verzoek
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// Verstuur het verzoek
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Fout: \(error)") // Hanteer fout
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Antwoord: \(string)") // Hanteer antwoord
    }
}

dataTask.resume()
```

De output zou de van de API teruggekregen gegevens moeten zijn, of een foutmelding als er iets misgaat.

## Diepere Duik

In de vroege dagen van het web was basisauthenticatie een snelle manier om bronnen te beveiligen. Door de eenvoud werd het breed toegepast ondanks dat het minder veilig is dan moderne alternatieven zoals OAuth, omdat inloggegevens niet versleuteld, maar alleen gecodeerd worden.

Naast basisauthenticatie bestaan er alternatieven zoals digestauthenticatie, API-sleutels, OAuth of JWT (JSON Web Tokens). Elk komt met voor- en nadelen rondom veiligheid, gebruiksgemak en het niveau van geboden bescherming.

Bij het verzenden van een HTTP-verzoek met basisauthenticatie is het de beste praktijk om ervoor te zorgen dat je HTTPS gebruikt, zodat je gecodeerde inloggegevens veilig worden verzonden. Ook is het aan te raden om inloggegevens niet hard te coderen; gebruik in plaats daarvan omgevingsvariabelen of veilige kluizen.

## Zie Ook

- [Apple's URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Auth RFC](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
