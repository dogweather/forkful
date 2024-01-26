---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:02:40.531034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med Basic Authentication innebär att du inkluderar användarens legitimationer i en header för att få tillgång till skyddade resurser på en server. Programmerare gör detta för att säkerställa att endast behöriga användare kan interagera med sensitiva delar av en webbtjänst.

## Så här gör du:
För att skicka en HTTP-begäran med Basic Authentication i Swift, använd URLSession. Här är ett exempel som demonstrerar processen:

```Swift
import Foundation

// Lagra användarnamn och lösenord
let username = "anvandare"
let password = "lösenord"

// Skapa en credentials-sträng och koda den i base64
if let credentialsData = "\(username):\(password)".data(using: .utf8) {
    let base64Credentials = credentialsData.base64EncodedString(options: [])
    
    // Skapa en URLRequest och inkludera header för Basic Authentication
    if let url = URL(string: "https://example.com/protected") {
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        request.setValue("Basic \(base64Credentials)", forHTTPHeaderField: "Authorization")
        
        // Skicka begäran
        let session = URLSession.shared
        let task = session.dataTask(with: request) { data, response, error in
            guard let data = data, error == nil else {
                print("Något gick fel: \(error?.localizedDescription ?? "Okänt fel")")
                return
            }
            if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
                if let responseString = String(data: data, encoding: .utf8) {
                    print("Svar från servern: \(responseString)")
                }
            } else {
                print("Begäran misslyckades, statuskod: \((response as? HTTPURLResponse)?.statusCode ?? 0)")
            }
        }
        task.resume()
    }
}
```

Om allt går som det ska bör du se serverns svar utskrivet i konsolen.

## Fördjupning
Basic Authentication har använts i HTTP sedan tidigt 90-tal och ingår i HTTP/1.0-specifikationen. Trots sin enkelhet är det mindre säkert jämfört med modernare autentiseringsmetoder som OAuth 2.0, på grund av att användaruppgifter skickas i klartext (endast base64-kodat). Dock är Basic Authentication fortfarande relevant för interna tjänster eller enklare API:er där hög säkerhet inte är lika kritisk.

Implementationen i Swift använder URLSession för att hantera HTTP-begäran och respons. Det är viktigt att nota att credentials alltid ska överföras över en säker anslutning (HTTPS) för att minimera risken för avlyssning. 

## Se även
- [Using URLSession in Swift](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
