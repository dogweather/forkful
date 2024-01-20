---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att HTTP-klienten inkluderar autentiseringsinformation i begäran för att verifiera rätten att få tillgång till specifika data eller funktioner. Det är användbart i programmeringssammanhang för att skydda känslig information från obehörig åtkomst.

## Så här gör du:
Nedan följer ett exempel på en HTTP-begäran med basic autentisering i Swift. Den använder `URLSession` och `URLRequest` för att ackomplishera detta.

```Swift
import Foundation

let credentials = "username:password"
let encodedCredentials = Data(credentials.utf8).base64EncodedString()

var request = URLRequest(url: URL(string: "http://example.com")!)
request.addValue("Basic \(encodedCredentials)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```
När koden ovan körs kommer den att skicka en HTTP-begäran till `http://example.com` med de angivna inloggningsuppgifterna i HTTP-headern `Authorization`.

## Djupdykning

**Historiskt sammanhang**
HTTP Basic Authentication är en av de ursprungliga autentiseringsmetoderna för HTTP, definierad i HTTP/1.0-specifikationen. Trots att det är enkelt gör det jobbet för enklare fall, men exponerar ofta känslig information som lösenord i klartext.

**Alternativ** 
Det finns många alternativ till HTTP Basic Authentication, som HTTP Digest Authentication, OAuth och JWT (Json Web Tokens) autentisering. Dessa metoder anses vara säkrare och mer flexibla än HTTP Basic Authentication.

**Implementeringsdetaljer**
I Swift är HTTP-begäran med grundläggande autentisering främst implementerad med `URLSession` och `URLRequest`. Kreditera som "username:password" konverteras till base64-format och läggs till i HTTP-headern som "Basic {encodedCredentials}".

## Se också
Du kan läsa mer om HTTP Basic Authentication och dess alternativ från följande källor:
1. [Mozilla HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [RFC 7617: The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
4. [Swift URLSession](https://developer.apple.com/documentation/foundation/urlsession)