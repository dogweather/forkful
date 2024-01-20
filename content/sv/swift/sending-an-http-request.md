---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Swift för att skicka HTTP-begäran: Ett dyk ner i detaljerna

## Vad & varför?

Att skicka en HTTP-begäran innebär en interaktion mellan klient och server över webben. Programmörer utför detta för att hämta eller skicka data till en server.

## Hur du

En grundsats för att skicka en HTTP-begäran i Swift kan se ut så här:

```Swift
import Foundation

let url = URL(string: "https://exemplary.org/api")!
var request = URLRequest(url: url)
request.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        print(String(data: data, encoding: .utf8))
    }
}
task.resume()
```

Exempel resultat skulle vara följande:
```Swift
{
    "message": "Begäran återvänd med framgång",
    ...
}
```

## Djupdykning

Historiskt sett har Swifts stöd för HTTP-nätverksbegäran utvecklats. Tidigare använde vi NSURLConnection, men sedan Swift 2.0 har URLSession varit det bästa valet.

Alternativen för HTTP-begäran inkluderar bibliotek som Alamofire, men för enkelhetens skull kan Swifts inbyggda URLSession vara tillräckligt. 

Viktiga detaljer om att implementera HTTP-begäran innefattar hanteringen av asynkrona operationer, eftersom nätverksbaserade operationer tar tid och vi inte vill blockera användarens interaktiva upplevelse. 

## Se även

- [URLSessions utvecklardokumentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Om HTTP-protokollet](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Overview)
- [Alamofire](https://github.com/Alamofire/Alamofire)