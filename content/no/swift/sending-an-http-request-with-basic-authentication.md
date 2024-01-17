---
title:                "Sending av en http-forespørsel med grunnleggende autentisering"
html_title:           "Swift: Sending av en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending av en http-forespørsel med grunnleggende autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sending av HTTP-forespørsler med grunnleggende autentisering handler om å sende informasjon til en nettside eller server ved å inkludere et brukernavn og passord. Dette gjøres for å sikre at bare autoriserte brukere har tilgang til informasjonen som sendes. Programmere bruker dette for å sikre at sensitiv informasjon ikke blir tatt av uautoriserte personer.

# Hvordan:
 ```Swift
 let url = URL(string: "https://www.example.com/login")!
 var request = URLRequest(url: url)
 request.httpMethod = "GET"
 let username = "username"
 let password = "password"
 let loginString = "\(username):\(password)"
 let loginData = loginString.data(using: .utf8)!
 let base64LoginString = loginData.base64EncodedString()
 request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
 let task = URLSession.shared.dataTask(with: request) { data, response, error in
     guard let data = data,
         let response = response as? HTTPURLResponse,
         error == nil
         else {                                             
             print("error", error ?? "Unknown error")
             return
         }
         print("status", response.statusCode)
         print("response", response)
         let responseString = String(data: data, encoding: .utf8)
         print("responseString", responseString ?? "No response")
     }
 task.resume()
 ```
Dette eksempelet viser hvordan du kan sende en HTTP GET-forespørsel med grunnleggende autentisering. Den inkluderer brukernavn og passord i forespørselen og dekoder dem til base64-koding før de blir sendt. Deretter kan du få tilgang til responsen og eventuelle data som kommer tilbake.

# Dypdykk:
Sending av HTTP-forespørsler med grunnleggende autentisering ble først standardisert i HTTP 1.0. Alternativer til dette inkluderer JWT-autentisering (JSON Web Token) og OAuth. Implementasjonsdetaljene kan variere avhengig av hvilket rammeverk eller nettverksbibliotek du bruker.

# Se også:
- [HTTP Basic Authentication Explained](https://www.digitalocean.com/community/tutorials/http-basic-authentication-explained)
- [Apple Developer Documentation for URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [The Evolution of HTTP Basic Authentication](https://dzone.com/articles/evolution-http-basic-authentication)