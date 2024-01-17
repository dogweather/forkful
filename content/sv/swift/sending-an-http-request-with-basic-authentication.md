---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Swift: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering är ett sätt för programmerare att säkert kommunicera med en server. Genom att kräva en användares lösenord vid varje förfrågan skyddas känslig information från obehöriga.

## How to:
Det första steget är att bygga en URL med adressen till den server eller API som du vill kommunicera med. Sedan måste du skapa en URLRequest-objekt med URL:en som argument och ange att förfrågan ska utföras med grundläggande autentisering. Slutligen måste du lägga till användarnamn och lösenord i en HTTP-header innan du skickar förfrågan.

```Swift
if let url = URL(string: "https://api.example.com") {
   var request = URLRequest(url: url)
   let credentials = "username:password".data(using: String.Encoding.utf8)
   let base64Credentials = credentials?.base64EncodedString(options: [])
   request.setValue("Basic \(base64Credentials)", forHTTPHeaderField: "Authorization")
   // execute HTTP request
}
```

## Deep Dive:
HTTP grundläggande autentisering är en äldre autentiseringsteknik som fortfarande används i dagens API:er och webbapplikationer. Alternativ till grundläggande autentisering inkluderar OAuth och JWT, som erbjuder bättre säkerhet och användarupplevelse. Vid implementering av grundläggande autentisering är det viktigt att hålla lösenordet säkert och använda HTTPS för att kryptera förfrågan.

## See Also:
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [OAuth](https://oauth.net/)
- [JWT](https://jwt.io/)