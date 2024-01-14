---
title:                "Swift: Skicka en http-förfrågan med grundläggande autentisering"
simple_title:         "Skicka en http-förfrågan med grundläggande autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

I en värld där allt är uppkopplat, är det viktigt för utvecklare att kunna skicka HTTP-förfrågningar med grundläggande autentisering. Detta gör det möjligt för applikationer att kommunicera med olika servrar på ett säkert sätt och få tillgång till skyddade resurser.

## Hur man gör

För att skicka en HTTP-förfrågan med grundläggande autentisering i Swift, måste vi först definiera en URL som vi vill ansluta till. Vi kan sedan skapa en URLRequest med denna URL och lägga till autentiseringsuppgifterna i form av en användarnamn och lösenord. Slutligen skickar vi förfrågan och hanterar eventuella fel. Se koden nedan för en enkel implementation:

```Swift
let urlString = "https://example.com/api"
let url = URL(string: urlString)
var request = URLRequest(url: url!)
request.httpMethod = "GET"

// Lägg till användarnamn och lösenord
let credentials = "\(username):\(password)"
let credentialsData = credentials.data(using: .utf8)
let base64Credentials = credentialsData!.base64EncodedString()
request.setValue("Basic \(base64Credentials)", forHTTPHeaderField: "Authorization")

// Skicka förfrågan
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
        return
    }

    // Hantera svaret
    if let httpResponse = response as? HTTPURLResponse {
        print("Statuskod: \(httpResponse.statusCode)")
        print("Svar: \(data)")
    }
}
task.resume() // Glöm inte att starta förfrågan
```

För att testa detta kan vi till exempel använda en fälla som [httpbin.org](http://httpbin.org/). Om vi gör en förfrågan till deras `/basic-auth` endpoint med rätt användarnamn och lösenord, bör vi få ett svar med statuskod 200 och vår autentiseringsuppgifter i svaret.

## Djupdykning

När vi lägger till autentisering med grundläggande autentisering i vår HTTP-förfrågan, skickas användarnamnet och lösenordet i en Base64-kodad sträng tillsammans med förfrågan. Det här är en enkel men inte helt säker autentiseringsteknik, eftersom autentiseringsuppgifterna är synliga om någon fångar vår förfrågan.

En bättre lösning är att använda ett mer avancerat autentiseringssystem som t.ex. OAuth. Men grundläggande autentisering är fortfarande ett praktiskt sätt att skydda vissa resurser i mindre skalade applikationer.

## Se även

- [Basic Authentication on Wikipedia](<https://en.wikipedia.org/wiki/Basic_access_authentication>)
- [HTTP Methods on MDN Web Docs](<https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods>)
- [URLSession on Apple Developer Documentation](<https://developer.apple.com/documentation/foundation/urlsession>)