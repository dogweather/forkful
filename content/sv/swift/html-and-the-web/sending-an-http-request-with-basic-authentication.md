---
date: 2024-01-20 18:02:40.531034-07:00
description: "Att skicka en HTTP-beg\xE4ran med Basic Authentication inneb\xE4r att\
  \ du inkluderar anv\xE4ndarens legitimationer i en header f\xF6r att f\xE5 tillg\xE5\
  ng till skyddade\u2026"
lastmod: 2024-02-19 22:04:57.490833
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran med Basic Authentication inneb\xE4r att du\
  \ inkluderar anv\xE4ndarens legitimationer i en header f\xF6r att f\xE5 tillg\xE5\
  ng till skyddade\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
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
